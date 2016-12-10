%%%----------------------------------------------------------------
%%% File        : btree_tests.erl
%%% Author      : Mikael Pettersson <mikael.pettersson@klarna.com>
%%% Description : Tests for Erlang implementation of B-tree sets
%%%
%%% Copyright (c) 2016 Klarna AB
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
%%%----------------------------------------------------------------

-module(btree_tests).
-export([io_init/0]).

-include_lib("eunit/include/eunit.hrl").

%% Exercise an order-2 B-tree using a sequence of insertions and deletions
%% taken from [Wirth76].
wirth_test() ->
  Keys = [20, 40, 10, 30, 15, 35, 7, 26, 18, 22, 5, 42, 13, 46, 27, 8, 32,
          38, 24, 45, 25],
  {ETS, IO} = io_init(),
  Btree1 = insert_all(IO, btree:new(2), Keys),
  Btree2 = delete_all(IO, Btree1, lists:reverse(Keys)),
  ?assertEqual([], btree:all_keys(IO, Btree2)),
  io_fini(ETS).

insert_all(IO, Btree, Keys) ->
  lists:foldl(fun (Key, Btree1) -> insert_one(IO, Btree1, Key) end,
              Btree, Keys).

insert_one(IO, Btree0, Key) ->
  Btree =
    case btree:insert(IO, Key, Btree0) of
      ok -> Btree0;
      {ok, Btree1} -> Btree1
    end,
  ?assertEqual(true, btree:member(IO, Key, Btree)),
  ?assertEqual(ok, btree:check(IO, Btree)),
  Btree.

delete_all(IO, Btree, Keys) ->
  lists:foldl(fun (Key, Btree1) -> delete_one(IO, Btree1, Key) end,
              Btree, Keys).

delete_one(IO, Btree0, Key) ->
  Btree =
    case btree:delete(IO, Key, Btree0) of
      ok -> Btree0;
      {ok, Btree1} -> Btree1
    end,
  ?assertEqual(false, btree:member(IO, Key, Btree)),
  ?assertEqual(ok, btree:check(IO, Btree)),
  Btree.

%% Use an ETS table to simulate secondary storage.

io_init() ->
  ETS = ets:new(undefined, [{keypos, 1}]),
  ets:insert(ETS, {next_pageid, 0}),
  {ETS, btree:mkio(ETS, fun page_read/2, fun page_write/3,
                   fun page_allocate/1, fun page_delete/2)}.

io_fini(ETS) ->
  ets:delete(ETS).

page_read(ETS, PageId) ->
  [{_PageId, Data}] = ets:lookup(ETS, PageId),
  {ok, Data}.

page_write(ETS, PageId, Data) ->
  ets:insert(ETS, {PageId, Data}),
  ok.

page_allocate(ETS) ->
  PageId = ets:update_counter(ETS, next_pageid, 1),
  {ok, PageId}.

page_delete(ETS, PageId) ->
  ets:delete(ETS, PageId),
  ok.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
