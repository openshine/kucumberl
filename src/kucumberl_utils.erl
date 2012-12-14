%%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%%% use this file except in compliance with the License. You may obtain a copy of
%%% the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%%% License for the specific language governing permissions and limitations under
%%% the License.
%%%-------------------------------------------------------------------
%%% @author Roberto Majadas <roberto.majadas@openshine.com>
%%% @copyright (C) 2012, Roberto Majadas
%%% @doc
%%%
%%% @end
%%% Created : 14 Dec 2012 by Roberto Majadas <roberto.majadas@openshine.com>
%%%-------------------------------------------------------------------
-module(kucumberl_utils).

%% API
-export([exit/1]).

%%%===================================================================
%%% API
%%%===================================================================

exit(Code) ->
    case erlang:is_builtin(erlang, halt, 2) of
        true ->
            halt(Code);
        false ->
            case os:type() of
                {win32, nt} ->
                    timer:sleep(100),
                    halt(Code);
                _ ->
                    halt(Code),
                    %% workaround to delay exit until all output is written
                    receive after infinity -> ok end
            end
    end.
