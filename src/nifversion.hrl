%%%----------------------------------------------------------------
%%%
%%% File        : nifversion.hrl
%%% Author      : Mikael Pettersson <mikael.pettersson@klarna.com>
%%% Description : Erlang implementation of B-tree sets
%%%
%%% Copyright (c) 2017 Klarna AB
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

%% To ensure that an updated NIF .so will be loaded and used when the
%% Erlang module is reloaded, it is necessary to add a version number
%% to the NIF .so file's name, and to bump that version number whenever
%% the NIF is updated.  This definition must be kept in sync with the
%% so_name in rebar.config.

-define(SONAME, "btree-1.0").

%%%_* Emacs -------------------------------------------------------
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
