%%%----------------------------------------------------------------
%%%
%%% File        : btree.erl
%%% Author      : Mikael Pettersson <mikael.pettersson@klarna.com>
%%% Description : Erlang implementation of B-tree sets
%%%
%%% Copyright (c) 2016-2017 Klarna AB
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%
%%%----------------------------------------------------------------
%%% 
%%% Implementation notes:
%%%
%%% - Initially based on [Wirth76].
%%% - Converted from Pascal to C, and then to Erlang.  The Erlang
%%%   version uses tuples in lieu of arrays.  Pages are of variable
%%%   not fixed size.
%%% - I/O of pages is made explicit.
%%% - Search is separated from insertion.  Search returns a stack of
%%%   pages and indices recording the path from the root to the key.
%%%   Insertion uses that stack to navigate the tree when rewriting it.
%%% - Deletion is tricky enough that the recursive structure of the
%%%   original code is kept as-is.  A local cache is used to avoid
%%%   redundant I/Os.
%%% - Our use cases only want sets, so this does not associate
%%%   attributes with the keys.
%%% - Bulk-loading into an empty tree has been added.
%%%
%%%----------------------------------------------------------------
%%%
%%% References:
%%%
%%% [BM72] R. Bayer and E. McCreight, "Organization and Maintenance of
%%% Large Ordered Indexes", Acta Informatica, Vol. 1, Fasc. 3, pp. 173--189,
%%% 1972.
%%%
%%% [Wirth76] Niklaus Wirth, "Algorithms + Data Structures = Programs",
%%% Program 4.7, pp 252--257, Prentice-Hall, 1976.
%%%
%%% [Comer79] Douglas Comer, "The Ubiquitous B-Tree", Computing Surveys,
%%% Vol. 11, No. 2, pp 121--137, ACM, June 1979.
%%%
%%%----------------------------------------------------------------

-module(btree).
-export([ new/1
        , from_list/3
        , member/3
        , all_keys/2
        , insert/3
        , delete/3
        , mkio/5
        ]).

-export([check/2, print/2]). % for debugging only

-export_type([btree/1]).

%%%_* Types and macros =========================================================

%-define(DEBUG, true).
-ifdef(DEBUG).
-define(dbg(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(dbg(Fmt, Args), ok).
-endif.

%% A page id is an integer > 0, or [].
%% In references to pages, [] denotes an absent page.
%% The root page is stored in the top-level #btree{}, and its page id is [].

-define(NOPAGEID, []).
-type pageid() :: pos_integer() | ?NOPAGEID.

%% An item is a pair of a key and a page reference.
%% FIXME: inline as 2 consecutive elements in the page's E vector

-type item() :: {K :: term(),
                 P :: pageid()}. % subtree with keys K' > K
-define(item(K, P), {K, P}).
item_p({_K, P}) -> P.
item_k({K, _P}) -> K.
item_set_p({K, _P}, P) -> {K, P}.
item_set_k({_K, P}, K) -> {K, P}.

%% A page is an array of m keys and m+1 page references, constrained
%% by m =< 2N, and m >= N except for the root page.

-record(page, % m == size(e)
        { pageid :: pageid()
        , p0 :: pageid() % subtree with keys k' < item_k(element(1, e))
        , e :: {item()}
        }).

-record(btree,
        { order :: pos_integer() % aka N
        , root :: #page{}
        }).

-type item(T) :: {K :: T, P :: pageid()}.
-type page(T) :: #page{e :: {item(T)}}.
-type btree(T) :: #btree{root :: page(T)}.

%%%_* Creating an empty B-tree =================================================

new(N) when is_integer(N), N >= 2 ->
  #btree{order = N,
         root = #page{
           pageid = ?NOPAGEID,
           p0 = ?NOPAGEID,
           e = {}}}.

%%%_* Bulk-loading a list into an empty B-tree =================================
%%% Citing from Wikipedia's article on B-trees, section "Initial construction":
%%%
%%% "In applications, it is frequently useful to build a B-tree to represent a
%%% large existing collection of data and then update it incrementally using
%%% standard B-tree operations. In this case, the most efficient way to
%%% construct the initial B-tree is not to insert every element in the initial
%%% collection successively, but instead to construct the initial set of leaf
%%% nodes directly from the input, then build the internal nodes from these.
%%% This approach to B-tree construction is called bulkloading. Initially, every
%%% leaf but the last one has one extra element, which will be used to build the
%%% internal nodes. We build the next level up from the leaves by taking the
%%% last element from each leaf node except the last one. Again, each node
%%% except the last will contain one extra value. This process is continued
%%% until we reach a level with only one node and it is not overfilled."

-record(item,
        { left % :: #node{} | []
        , key :: term()
        }).
-record(node,  {e :: [#item{}], right :: #node{} | []}).
-record(xnode, {e :: [#item{}], xtra :: #item{}}).

from_list(IO, N, Keys) when is_integer(N), N >= 2 ->
  to_root(IO, N, make_tree(N, make_leaves(N, lists:usort(Keys)))).

%% Pass 1: Reduce the list of keys to a left-centric tree.  (Items have a "left"
%% reference to subtree with smaller keys, nodes have a "right" reference to
%% subtree with keys larger than the largest key in the node.)

make_leaves(N, Keys) -> % -> {[#xnode{}], #node{}}
  make_xnodes(Keys, fun key_to_item/1, [], length(Keys), N, []).

make_tree(N, {XNodes, LastNode}) -> % -> #node{}
  Length = length(XNodes),
  if Length > N*2 ->
      make_tree(N, make_xnodes(XNodes, fun xnode_to_item/1, LastNode, Length,
                               N, []));
     true ->
      E = [xnode_to_item(XNode) || XNode <- XNodes],
      make_node(E, LastNode)
  end.

%% -> {[#xnode{}], #node{}}
make_xnodes(Things, ThingToItem, LastNode, Length, N, Acc) ->
  if Length > N*2 ->
      {Left, [Xtra | Right]} = lists:split(N*2, Things),
      E = [ThingToItem(Thing) || Thing <- Left],
      X = ThingToItem(Xtra),
      make_xnodes(Right, ThingToItem, LastNode, Length - (N*2 + 1), N,
                  [#xnode{e = E, xtra = X} | Acc]);
     true ->
      E = [ThingToItem(Thing) || Thing <- Things],
      {lists:reverse(Acc), make_node(E, LastNode)}
  end.

key_to_item(Key) -> #item{left = [], key = Key}.

xnode_to_item(#xnode{e = E, xtra = #item{left = Left, key = Key}}) ->
  #item{left = make_node(E, Left), key = Key}.

make_node(_E = [], Right) -> Right;
make_node(E, Right) -> #node{e = E, right = Right}.

%% Pass 2: Map the left-centric tree to a right-centric B-tree, and output it.

to_root(_IO, N, []) ->
  new(N);
to_root(IO, N, #node{e = E, right = R}) ->
  #btree{order = N, root = to_page(IO, ?NOPAGEID, E, R)}.

to_page(IO, PageId, [#item{left = L, key = K} | E], R) ->
  P0 = to_pageid(IO, L),
  to_page(IO, PageId, P0, K, E, R, []).

to_page(IO, PageId, P0, K, [#item{left = L, key = K1} | E], R, Acc) ->
  Item = ?item(K, to_pageid(IO, L)),
  to_page(IO, PageId, P0, K1, E, R, [Item | Acc]);
to_page(IO, PageId, P0, K, [], R, Acc) ->
  Item = ?item(K, to_pageid(IO, R)),
  E = list_to_tuple(lists:reverse([Item | Acc])),
  #page{pageid = PageId, p0 = P0, e = E}.

to_pageid(_IO, []) ->
  ?NOPAGEID;
to_pageid(IO, #node{e = E, right = R}) ->
  PageId = page_allocate(IO),
  Page = to_page(IO, PageId, E, R),
  page_write(IO, Page),
  PageId.

%%%_* Membership check =========================================================
%%%
%%% Return true if a key is present in a B-tree, false otherwise.

member(IO, X, #btree{root = A}) ->
  case search(IO, X, A) of
    {true, _P, _K, _Path} -> true;
    {false, _Path} -> false
  end.

%%%_* B-tree Search ============================================================
%%%
%%% Search a B-tree for key X.
%%% Return {true, P, K, Path} if found, where P is the page containing X
%%% at index K, and Path is a list of {Q,R} pairs listing, in reverse, the
%%% sequence of pages and indexes followed from the root to P.
%%% If not found, return {false, Path}.

search(IO, X, A) ->
  search_page(IO, X, A, []).

search_pageid(_IO, _X, ?NOPAGEID, Path) ->
  {false, Path};
search_pageid(IO, X, A, Path) ->
  search_page(IO, X, page_read(IO, A), Path).

search_page(IO, X, P = #page{p0 = P0, e = E}, Path) ->
  case binsearch(E, X) of
    {found, K} ->
      {true, P, K, Path};
    {not_found, R} ->
      Q =
        if R =:= 0 -> P0;
           true -> item_p(element(R, E))
        end,
      search_pageid(IO, X, Q, [{P, R} | Path])
  end.

%%%_* Binary search within a page ==============================================
%%%
%%% Search a page's item vector E for key X.
%%% Return {found, K} if found at index K,
%%% otherwise {not_found, R} [FIXME: proper defn of R].

binsearch(E, X) ->
  binsearch(E, X, 1, size(E)).
binsearch(E, X, L, R) when R >= L ->
  K = (L + R) div 2,
  KX = item_k(element(K, E)),
  if X < KX -> binsearch(E, X, L, K - 1);
     X =:= KX -> {found, K};
     true -> binsearch(E, X, K + 1, R)
  end;
binsearch(_E, _X, _L, R) ->
  {not_found, R}.

%%%_* B-tree all_keys ==========================================================

all_keys(IO, #btree{root = A}) ->
  all_keys_page(IO, A, []).

all_keys_pageid(_IO, ?NOPAGEID, L) -> L;
all_keys_pageid(IO, A, L) ->
  all_keys_page(IO, page_read(IO, A), L).

all_keys_page(IO, #page{p0 = P0, e = E}, L) ->
  all_keys_elements(IO, E, 1, all_keys_pageid(IO, P0, L)).

all_keys_elements(_IO, E, I, L) when I > size(E) -> L;
all_keys_elements(IO, E, I, L) ->
  ?item(K, P) = element(I, E),
  all_keys_elements(IO, E, I + 1, all_keys_pageid(IO, P, [K | L])).

%%%_* Insertion into a B-tree ==================================================

insert(IO, X, Btree = #btree{order = N, root = Root}) ->
  case search(IO, X, Root) of
    {true, _P, _K, _Path} ->
      %% Nothing to do for sets.  For mappings, update the page if the
      %% key's attributes have changed.
      ok;
    {false, Path} ->
      U = ?item(X, ?NOPAGEID),
      case insert(IO, N, ?NOPAGEID, U, Path) of
        false ->
          ok;
        NewRoot ->
          {ok, Btree#btree{root = NewRoot}}
      end
  end.

insert(_IO, _N, P0, U, []) ->
  ?dbg("insert(~p, ~p, ~p, ~p)~n", [_N,P0,U,[]]),
  #page{pageid = ?NOPAGEID, p0 = P0, e = {U}};
insert(IO, N, _P0, U, [{A, R} | Path]) ->
  ?dbg("insert(~p, ~p, ~p, ~p)~n", [N,U,_P0,[{A,R}|Path]]),
  E = A#page.e,
  if size(E) < 2 * N ->
      %% Page A has room for U.
      E2 = erlang:insert_element(R + 1, E, U),
      A2 = A#page{e = E2},
      case A2#page.pageid of
        ?NOPAGEID ->
          %% updating the root: return new version
          A2;
        _ ->
          page_write(IO, A2),
          false
      end;
     true ->
      %% Page A is full; split it and continue with the emerging item V.
      {V, AE, BE} = split(N, U, E, R),
      ?dbg("split(~p, ~p, ~p, ~p)~n-> {~p, ~p, ~p}~n", [N,U,E,R,V,AE,BE]),
      A2 =
        case A#page.pageid of
          ?NOPAGEID ->
            %% splitting old root, allocate page for left half
            A#page{e = AE, pageid = page_allocate(IO)};
          _ ->
            A#page{e = AE}
        end,
      BPageId = page_allocate(IO),
      B = #page{pageid = BPageId, p0 = item_p(V), e = BE},
      page_write(IO, A2),
      page_write(IO, B),
      insert(IO, N, A2#page.pageid, item_set_p(V, BPageId), Path)
  end.

split(N, U, E, R) ->
  if R =:= N ->
      {AE, BE} = lists:split(N, tuple_to_list(E)),
      {U, list_to_tuple(AE), list_to_tuple(BE)};
     R < N ->
      %% insert U in left page
      V = element(N, E),
      {AE, [_V | BE]} = lists:split(N - 1, tuple_to_list(E)),
      {AE1, AE2} = lists:split(R, AE),
      {V, list_to_tuple(AE1 ++ [U] ++ AE2), list_to_tuple(BE)};
     true ->
      %% insert U in right page
      R2 = R - N,
      V = element(N + 1, E),
      {AE, [_V | BE]} = lists:split(N, tuple_to_list(E)),
      {BE1, BE2} = lists:split(R2 - 1, BE),
      {V, list_to_tuple(AE), list_to_tuple(BE1 ++ [U] ++ BE2)}
  end.

%%%_* Deletion in a B-tree =====================================================
%%%
%%% Deletion sometimes inspects and updates pages multiple times, so we use a
%%% simple cache to avoid redundant I/O operations here.

delete(IO, X, Btree = #btree{order = N, root = Root}) ->
  Cache1 = mkcache(IO, Root),
  Cache2 = delete_1(Cache1, X, N, Root),
  State = cache_state(Cache2, Root#page.pageid),
  cache_flush(Cache2),
  case State of
    {_PageId, clean, _Root} -> ok;
    {_PageId, dirty, NewRoot} -> {ok, Btree#btree{root = NewRoot}}
  end.

delete_1(Cache1, X, N, Root) ->
  RootPageId = Root#page.pageid,
  case delete(Cache1, N, X, RootPageId, Root) of
    {Cache2, true} ->
      %% base page size was reduced
      {Cache3, #page{p0 = P0, e = E}} = cache_read(Cache2, RootPageId),
      if size(E) =:= 0 ->
          %% Wirth's original code would delete the old root page and
          %% redirect the btree to P0.  We don't have a separate root
          %% page so we can't do that.  Instead we copy the contents
          %% of P0 into the root, and delete P0.
          case P0 of
            ?NOPAGEID ->
              Cache3;
            _ ->
              {Cache4, A} = cache_read(Cache3, P0),
              Cache5 = cache_write(Cache4, A#page{pageid = RootPageId}),
              Cache6 = cache_delete(Cache5, P0),
              Cache6
          end;
         true ->
          Cache3
      end;
    {Cache2, false} ->
      Cache2
  end.

%%% Search and delete key X in B-tree A; if a page underflow is
%%% necessary, balance with adjacent page if possible, otherwise merge;
%%% return true if page A becomes undersize.
delete(Cache, _N, _X, ?NOPAGEID) ->
  ?dbg("delete(~p, ~p)~n", [_X, ?NOPAGEID]),
  {Cache, false};
delete(Cache1, N, X, APageId) ->
  {Cache2, A} = cache_read(Cache1, APageId),
  delete(Cache2, N, X, APageId, A).

delete(Cache2, N, X, APageId, A = #page{p0 = AP0, e = AE}) ->
  ?dbg("delete(~p, ~p)~n", [X, A]),
  case binsearch(AE, X) of
    {found, K} ->
      %% found, now delete a^.e[k]
      R = K - 1,
      QPageId =
        if R =:= 0 -> AP0;
           true -> item_p(element(R, AE))
        end,
      if QPageId =:= ?NOPAGEID ->
          %% a is a terminal page
          AE2 = erlang:delete_element(K, AE),
          Cache3 = cache_write(Cache2, A#page{e = AE2}),
          {Cache3, size(AE2) < N};
         true ->
          case del(Cache2, N, QPageId, APageId, K) of
            {Cache3, true} ->
              underflow(Cache3, N, APageId, QPageId, R);
            {Cache3, false} ->
              {Cache3, false}
          end
      end;
    {not_found, R} ->
      QPageId =
        if R =:= 0 -> AP0;
           true -> item_p(element(R, AE))
        end,
      case delete(Cache2, N, X, QPageId) of
        {Cache3, true} ->
          underflow(Cache3, N, APageId, QPageId, R);
        {Cache3, false} ->
          {Cache3, false}
      end
  end.

del(Cache1, N, PPageId, APageId, K) ->
  {Cache2, P = #page{e = PE}} = cache_read(Cache1, PPageId),
  ?dbg("del(~p, ~p, ~p)~n", [P, APageId, K]),
  PEM = element(size(PE), PE),
  QPageId = item_p(PEM),
  if QPageId =/= ?NOPAGEID ->
      case del(Cache2, N, QPageId, APageId, K) of
        {Cache3, true} ->
          underflow(Cache3, N, PPageId, QPageId, size(PE));
        {Cache3, false} ->
          {Cache3, false}
      end;
     true ->
      {Cache3, A = #page{e = AE}} = cache_read(Cache2, APageId),
      %% Wirth's code does several redundant assignments in this case.
      AE2 = setelement(K, AE, item_set_k(element(K, AE), item_k(PEM))),
      PE2 = erlang:delete_element(size(PE), PE),
      Cache4 = cache_write(Cache3, A#page{e = AE2}),
      Cache5 = cache_write(Cache4, P#page{e = PE2}),
      {Cache5, size(PE2) < N}
  end.

%% Page A, referenced from page C at index S, is undersize (size(E) == N-1).
%% Fix that by borrowing from or merging with an adjacent page B.
%% Return true if C becomes undersize, false otherwise.
underflow(Cache1, N, CPageId, APageId, S) ->
  {Cache2, C = #page{p0 = CP0, e = CE}} = cache_read(Cache1, CPageId),
  {Cache3, A = #page{p0 = AP0, e = AE}} = cache_read(Cache2, APageId),
  ?dbg("underflow(~p, ~p, ~p)~n", [C, A, S]),
  if S < size(CE) ->
      %% b = page to the right of a
      S1 = S + 1,
      BPageId = item_p(element(S1, CE)),
      {Cache4, B = #page{p0 = BP0, e = BE}} = cache_read(Cache3, BPageId),
      MB = size(BE),
      K = (MB - N + 1) div 2,
      %% k = no. of items available on adjacent page b
      U = ?item(item_k(element(S1, CE)), BP0),
      if K > 0 ->
          %% move k items from b to a
          %% (actually only k-1 items, 1 is moved to c)
          {BE1, [?item(BEKk, BEKp) | BE2]} =
            lists:split(K - 1, tuple_to_list(BE)),
          AE2 = list_to_tuple(tuple_to_list(AE) ++ [U] ++ BE1),
          CE2 = setelement(S1, CE, ?item(BEKk, BPageId)),
          Cache5 = cache_write(Cache4, A#page{e = AE2}),
          Cache6 = cache_write(Cache5, C#page{e = CE2}),
          Cache7 =
            cache_write(Cache6, B#page{p0 = BEKp, e = list_to_tuple(BE2)}),
          {Cache7, false};
         true ->
          %% merge pages a and b
          AE2 = list_to_tuple(tuple_to_list(AE) ++ [U] ++ tuple_to_list(BE)),
          CE2 = erlang:delete_element(S1, CE),
          Cache5 = cache_write(Cache4, A#page{e = AE2}),
          Cache6 = cache_write(Cache5, C#page{e = CE2}),
          Cache7 = cache_delete(Cache6, BPageId),
          {Cache7, size(CE2) < N}
      end;
     true ->
      %% b = page to the left of a
      BPageId =
        if S =:= 1 -> CP0;
           true -> item_p(element(S - 1, CE))
        end,
      {Cache4, B = #page{e = BE}} = cache_read(Cache3, BPageId),
      MB = size(BE) + 1,
      K = (MB - N) div 2,
      if K > 0 ->
          %% move k items from page b to a
          U = ?item(item_k(element(S, CE)), AP0),
          MB2 = MB - K,
          {BE1, [?item(BEMB2k, BEMB2p) | BE2]}
            = lists:split(MB2 - 1, tuple_to_list(BE)),
          AE2 = list_to_tuple(BE2 ++ [U] ++ tuple_to_list(AE)),
          CE2 = setelement(S, CE, ?item(BEMB2k, APageId)),
          Cache5 = cache_write(Cache4, A#page{p0 = BEMB2p, e = AE2}),
          Cache6 = cache_write(Cache5, B#page{e = list_to_tuple(BE1)}),
          Cache7 = cache_write(Cache6, C#page{e = CE2}),
          {Cache7, false};
         true ->
          %% merge pages a and b
          U = ?item(item_k(element(S, CE)), AP0),
          BE2 = list_to_tuple(tuple_to_list(BE) ++ [U] ++ tuple_to_list(AE)),
          Cache5 = cache_write(Cache4, B#page{e = BE2}),
          Cache6 =
            cache_write(Cache5, C#page{e = erlang:delete_element(S, CE)}),
          Cache7 = cache_delete(Cache6, APageId),
          {Cache7, (S - 1) < N}
      end
  end.

%%%_* Page I/O cache for delete ================================================
%%%
%%% The B-trees are expected to be shallow, so the number of pages touched
%%% during delete will be few.  Therefore, the cache is simply a list.
%%%
%%% INV: There is at most one entry per PageId in the cache.

-record(cache, {io, entries}).

mkcache(IO, Root) ->
  #cache{io = IO, entries = [{Root#page.pageid, clean, Root}]}.

cache_state(#cache{entries = Entries}, PageId) ->
  lists:keyfind(PageId, 1, Entries).

cache_flush(#cache{io = IO, entries = Entries}) ->
  [case Entry of
     {_PageId, clean, _Page} -> ok;
     {_PageId, dirty, Page} -> page_write(IO, Page);
     {PageId, deleted} -> page_delete(IO, PageId)
   end || Entry <- Entries],
  ok.

cache_read(Cache = #cache{io = IO, entries = Entries}, PageId) ->
  case lists:keyfind(PageId, 1, Entries) of
    {_PageId, clean, Page} -> {Cache, Page};
    {_PageId, dirty, Page} -> {Cache, Page};
    %% deliberate crash if deleted
    false ->
      Page = page_read(IO, PageId),
      NewEntries = [{PageId, clean, Page} | Entries],
      {Cache#cache{entries = NewEntries}, Page}
  end.

cache_write(Cache = #cache{entries = Entries},
            Page = #page{pageid = PageId}) ->
  NewEntries = lists:keystore(PageId, 1, Entries, {PageId, dirty, Page}),
  Cache#cache{entries = NewEntries}.

cache_delete(Cache = #cache{entries = Entries}, PageId) ->
  NewEntries = lists:keystore(PageId, 1, Entries, {PageId, deleted}),
  Cache#cache{entries = NewEntries}.

%%%_* Page I/O =================================================================
%%%
%%% I/O is done by client-provided callbacks.

-record(io,
        { handle
        , read
        , write
        , allocate
        , delete
        }).

mkio(Handle, Read, Write, Allocate, Delete) ->
  #io{handle = Handle, read = Read, write = Write, allocate = Allocate,
      delete = Delete}.

page_read(#io{handle = Handle, read = Read}, PageId) ->
  ?dbg("page_read ~p~n", [PageId]),
  {ok, {P0, E}} = Read(Handle, PageId),
  #page{pageid = PageId, p0 = P0, e = E}.

page_write(#io{handle = Handle, write = Write}, Page) ->
  #page{pageid = PageId, p0 = P0, e = E} = Page,
  ?dbg("page_write ~p~n", [Page]),
  ok = Write(Handle, PageId, {P0, E}).

page_allocate(#io{handle = Handle, allocate = Allocate}) ->
  {ok, PageId} = Allocate(Handle),
  ?dbg("page_allocate = ~p~n", [PageId]),
  PageId.

page_delete(#io{handle = Handle, delete = Delete}, PageId) ->
  ?dbg("page_delete ~p~n", [PageId]),
  ok = Delete(Handle, PageId).

%%%_* Checking a B-tree (for debugging) ========================================
%%% Normally we'd only permit the root node to be underfilled, but bulk-looading
%%% tends to leave the right-most nodes underfilled, so we permit that too.

check(IO, #btree{order = N, root = A}) ->
  LowerBound = false,
  PermitUnderfill = true,
  _LowerBound2 = check_page(IO, N, A, LowerBound, PermitUnderfill),
  ok.

check_pageid(_IO, _N, ?NOPAGEID, LowerBound, _PermitUnderfill) ->
  LowerBound;
check_pageid(IO, N, PageId, LowerBound, PermitUnderfill) ->
  check_page(IO, N, page_read(IO, PageId), LowerBound, PermitUnderfill).

check_page(IO, N, #page{p0 = P0, e = E}, LowerBound, PermitUnderfill) ->
  %% check page size
  true = size(E) =< 2 * N,
  true = PermitUnderfill orelse size(E) >= N,
  %% check key order and subtrees
  LowerBound2 = check_pageid(IO, N, P0, LowerBound, false),
  check_elements(IO, N, E, 1, LowerBound2, PermitUnderfill).

check_elements(_IO, _N, E, I, LowerBound, _PermitUnderfill) when I > size(E) ->
  LowerBound;
check_elements(IO, N, E, I, LowerBound, PermitUnderfill) ->
  ?item(K, P) = element(I, E),
  check_key(K, LowerBound),
  LowerBound2 = {ok, K},
  PermitUnderfill2 = PermitUnderfill andalso I =:= size(E),
  LowerBound3 = check_pageid(IO, N, P, LowerBound2, PermitUnderfill2),
  check_elements(IO, N, E, I + 1, LowerBound3, PermitUnderfill).

check_key(_K, false) -> true;
check_key(K, {ok, LowerBound}) -> true = K > LowerBound.

%%%_* Printing a B-tree (for debugging) ========================================

print(IO, #btree{root = A}) ->
  print_page(IO, A, 1).

print_pageid(_IO, ?NOPAGEID, _L) ->
  ok;
print_pageid(IO, PageId, L) ->
  print_page(IO, page_read(IO, PageId), L).

print_page(IO, #page{p0 = P0, e = E}, L) ->
  [io:format("     ") || _I <- lists:seq(1, L)],
  [io:format(" ~4w", [item_k(element(I, E))]) || I <- lists:seq(1, size(E))],
  io:format("~n"),
  print_pageid(IO, P0, L + 1),
  [print_pageid(IO, item_p(element(I, E)), L + 1)
   || I <- lists:seq(1, size(E))],
  ok.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
