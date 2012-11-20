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
%%% @copyright (C) 2012, OpenShine S.L.
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2012 by Roberto Majadas <roberto.majadas@openshine.com>
%%%-------------------------------------------------------------------
-module(kucumberl_feature).
-include("kucumberl.hrl").

%% API
-export([run/1]).

%%%===================================================================
%%% API
%%%===================================================================

run(Feature) ->
    kucumberl_feature_code:load(Feature),
    kucumberl_log:init_feature(Feature),
    F = run_feature(Feature),
    kucumberl_log:end_feature(),
    kucumberl_feature_code:unload(F).

%%%===================================================================
%%% Internal functions
%%%===================================================================

run_feature(Feature) ->
    lists:foldl(
      fun(ScnID, F) ->
	      kucumberl_log:init_scenario(ScnID),
	      F = kucumberl_feature_scn:run(F, ScnID),
	      kucumberl_log:end_scenario(),
	      F
      end,
      Feature,
      lists:seq(1, length(Feature#feature.scenarios))).


