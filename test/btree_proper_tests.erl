%%%----------------------------------------------------------------
%%% File        : btree_proper_tests.erl
%%% Author      : Mikael Pettersson <mikael.pettersson@klarna.com>
%%% Description : Proper tests for Erlang implementation of B-tree sets
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

-module(btree_proper_tests).

-behaviour(proper_statem).

-export([ test/0
        , test/1
        , prop_seq/0
        ]).

%% statem callbacks
-export([ initial_state/0
        , command/1
        , precondition/2
        , postcondition/3
        , next_state/3
        ]).

%% command callbacks
-export([ member/1
        , insert/1
        , delete/1
        , all_keys/0
        ]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(st, {set}).

test() ->
  test(100).

test(N) ->
  true = proper:quickcheck(?MODULE:prop_seq(), N),
  ok.

prop_seq() ->
  ?FORALL(Cmds, proper_statem:commands(?MODULE),
          begin
            setup(),
            {History, State, Result} = proper_statem:run_commands(?MODULE, Cmds),
            cleanup(),
            ?WHENFAIL(
               io:fwrite("History: ~w~n"
                         "State  : ~w~n"
                         "Result : ~w~n", [History, State, Result]),
               proper:aggregate(
                 proper_statem:command_names(Cmds), Result =:= ok))
          end).

%% statem callbacks.

initial_state() ->
  #st{set = sets:new()}.

command(#st{}) ->
  proper_types:oneof([ {call, ?MODULE, member, [key()]}
                     , {call, ?MODULE, insert, [key()]}
                     , {call, ?MODULE, delete, [key()]}
                     , {call, ?MODULE, all_keys, []}
                     ]).

key() ->
  proper_types:oneof(lists:seq(1, 1000)).

precondition(_St, _Cmd) ->
  true.

postcondition(#st{set = Set}, {call, ?MODULE, member, [Key]}, {_IO, _Btree, Result}) ->
  sets:is_element(Key, Set) =:= Result;
postcondition(_St, {call, ?MODULE, insert, [Key]}, {IO, Btree}) ->
  valid(IO, Btree) andalso btree:member(IO, Key, Btree);
postcondition(_St, {call, ?MODULE, delete, [Key]}, {IO, Btree}) ->
  valid(IO, Btree) andalso not btree:member(IO, Key, Btree);
postcondition(#st{set = Set}, {call, ?MODULE, all_keys, []}, {_IO, _Btree, Result}) ->
  lists:sort(sets:to_list(Set)) =:= lists:sort(Result).

valid(IO, Btree) ->
  try
    btree:check(IO, Btree),
    true
  catch {error, badmatch} ->
    false
  end.

next_state(St, _Result, {call, ?MODULE, member, [_Key]}) ->
  St;
next_state(St = #st{set = Set}, _Result, {call, ?MODULE, insert, [Key]}) ->
  St#st{set = sets:add_element(Key, Set)};
next_state(St = #st{set = Set}, _Result, {call, ?MODULE, delete, [Key]}) ->
  St#st{set = sets:del_element(Key, Set)};
next_state(St, _Result, {call, ?MODULE, all_keys, []}) ->
  St.

%% Command callbacks.

member(Key) ->
  {IO, Btree} = get_io_btree(),
  {IO, Btree, btree:member(IO, Key, Btree)}.

insert(Key) ->
  {IO, Btree0} = get_io_btree(),
  case btree:insert(IO, Key, Btree0) of
    ok -> {IO, Btree0};
    {ok, Btree1} -> put_btree(Btree1), {IO, Btree1}
  end.

delete(Key) ->
  {IO, Btree0} = get_io_btree(),
  case btree:delete(IO, Key, Btree0) of
    ok -> {IO, Btree0};
    {ok, Btree1} -> put_btree(Btree1), {IO, Btree1}
  end.

all_keys() ->
  {IO, Btree} = get_io_btree(),
  {IO, Btree, btree:all_keys(IO, Btree)}.

%% Use the process dictionary to pass around the Btree state.

setup() ->
  {ETS, IO} = io_init(),
  put(?MODULE, {btree:new(2), IO, ETS}).

cleanup() ->
  {_Btree, _IO, ETS} = get(?MODULE),
  io_fini(ETS),
  erase(?MODULE).

put_btree(Btree) ->
  {_Btree, IO, ETS} = get(?MODULE),
  put(?MODULE, {Btree, IO, ETS}).

get_io_btree() ->
  {Btree, IO, _ETS} = get(?MODULE),
  {IO, Btree}.

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
