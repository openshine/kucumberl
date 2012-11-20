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
%%% Created : 19 Nov 2012 by Roberto Majadas <roberto.majadas@openshine.com>
%%%-------------------------------------------------------------------
-module(kucumberl_log).
-include("kucumberl.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
	 set_color/1,
	 init_feature/1,
	 init_scenario/1,
	 end_feature/0,
	 end_scenario/0,
	 emit/2,
	 print_stats/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  feature = [],
	  scnid = [],
	  color = true
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_color(Value) ->
    gen_server:call(?MODULE, {set_color, Value}).

init_feature(Feature) ->
    gen_server:call(?MODULE, {init_feature, Feature}).

end_feature() ->
    gen_server:call(?MODULE, {end_feature}).

init_scenario(ScnID) ->
    gen_server:call(?MODULE, {init_scenario, ScnID}).

end_scenario() ->
    gen_server:call(?MODULE, {end_scenario}).

emit(Event, Params) ->
    gen_server:call(?MODULE, {event, Event, Params}).

print_stats() ->
    gen_server:call(?MODULE, {print_stats}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({init_feature, Feature}, _From, State) ->
    NewState = State#state{feature = Feature},
    print_feature_name(NewState),
    {reply, ok, NewState};

handle_call({set_color, Value}, _From, State) ->
    case Value of
	true ->
	    NewState = State#state{color = true};
	false ->
	    NewState = State#state{color = false}
    end,
    {reply, ok, NewState};

handle_call({end_feature}, _From, State) ->
    NewState = State#state{feature = []},
    {reply, ok, NewState};

handle_call({init_scenario, ScnID}, _From, State) ->
    NewState = State#state{scnid = ScnID},
    print_scenario_name(NewState),
    {reply, ok, NewState};

handle_call({end_scenario}, _From, State) ->
    NewState = State#state{scnid = []},
    io:format("~n"),
    {reply, ok, NewState};

handle_call({event, Event, Params}, _From, State) ->
    print_event(State, Event, Params),
    {reply, ok, State};

handle_call({print_stats}, _From, State) ->
    print_full_stats(State),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

print_feature_name(State) ->
    F = State#state.feature,
    io:format("Feature: ~s~n", [F#feature.desc]).

print_scenario_name(State) ->
    F = State#state.feature,
    Scn = lists:nth(State#state.scnid,
		    F#feature.scenarios),
    io:format("  Scenario: ~s~n", [Scn#scenario.desc]).

print_event(State, init_step, {Type, ScnID, EID, ActID}) ->
    F = State#state.feature,
    Scn = case Type of
	      normal ->
		  lists:nth(State#state.scnid,
			    F#feature.scenarios);
	      background ->
		  F#feature.background
	  end,
    Act = lists:nth(ActID,
		    Scn#scenario.actions),
    print_step(Type, State, ScnID, EID, ActID, Act);

print_event(State, end_step, {Type, ScnID, EID, ActID}) ->
    F = State#state.feature,
    Scn = case Type of
	      normal ->
		  lists:nth(State#state.scnid,
			    F#feature.scenarios);
	      background ->
		  F#feature.background
	  end,
    Act = lists:nth(ActID,
		    Scn#scenario.actions),
    PAct = prepare_act(Scn, EID, Act),
    print_step_with_result(Type, State, ScnID, EID, ActID, PAct),
    print_step_extra_data(Type, State, ScnID, EID, ActID, PAct);

print_event(_,_,_) -> ignoreit.

print_step(Type, State, ScnID, EID, ActID, Act) ->
    AnsiEsc = esc_helper(save_cursor),
    S1 = tab_level(2),
    SE = example_str(Type, State, ScnID, EID, ActID, Act),
    S2 = case Type of
	     background -> background_str();
	     normal -> ""
	 end,
    S3 = step2str(Act#action.step) ++ " " ++ Act#action.desc,
    io:format("~s", [AnsiEsc ++ S1 ++ SE ++ S2 ++ S3]).

print_step_with_result(Type, State, ScnID, EID, ActID, Act) ->
    F = State#state.feature,
    AnsiEsc = esc_helper(restore_cursor) ++ esc_helper(erase_line),
    S1 = tab_level(2),
    SE = example_str(Type, State, ScnID, EID, ActID, Act),
    S2 = case Type of
	     background -> background_str();
	     normal -> ""
	 end,
    S3 = step2str(Act#action.step) ++ " " ++ Act#action.desc,
    [[R]] = ets:match(kctx, {{F#feature.id, Type, ScnID, EID, ActID}, '$1'}),
    SR = case R of
	     ok -> ok_str(State);
	     {failed, _Reason} -> failed_str(State);
	     not_implementated -> not_impl_str(State);
	     disabled -> disabled_str(State)
	 end,
    io:format("~s~s~s~n", [AnsiEsc,
			   string:left(S1++SE++S2++S3, 65),
			   SR]).

print_step_extra_data(Type, State, ScnID, EID, ActID, Act) ->
    S1 = tab_level(2),
    SE = example_str(Type, State, ScnID, EID, ActID, Act),

    case Act#action.text of
	[] -> ignoreit;
	T -> io:format("~s \"\"\"~n", [S1++SE]),
	     lists:foreach(
	       fun (L) ->
		       io:format("~s ~s~n", [S1++SE, L])
	       end,
	       re:split(T, "\r\n|\n|\r|\032", [{return, list}])),
	     io:format("~s \"\"\"~n", [S1++SE])
    end,

    case Act#action.tabletxt of
	"" -> ignoreit;
	TT -> lists:foreach(
	       fun (L) ->
		       io:format("~s |~s|~n", [S1++SE, L])
	       end,
	      re:split(TT, "\r\n|\n|\r|\032", [{return, list}]))
    end.


prepare_act(Scn, EID, Act) ->
    case Scn#scenario.examples of
	[] -> Act;
	_ ->
	    HRow = lists:nth(1, Scn#scenario.examples),
	    ERow = lists:nth(EID + 1, Scn#scenario.examples),
	    try lists:zip(HRow, ERow) of
		E ->
		    lists:foldl(
		      fun ({K,V}, A) ->
			      NewDesc = re:replace(A#action.desc,
						   "<" ++ K ++ ">",
						   V,
						   [{return, list}]),
			      A#action{desc = NewDesc}
		      end, Act, E)
	    catch
		_ -> Act
	    end
    end.

example_str(Type, State, ScnID, EID, ActID, _Act) ->
    F = State#state.feature,
    Scn = lists:nth(ScnID, F#feature.scenarios),
    case Scn#scenario.examples of
	[] -> "";
	_ -> case F#feature.background of
		 [] ->
		     case Type of
			 normal when ActID =:= 1 ->
			     "Ex." ++ string:right(integer_to_list(EID), 2) ++ "| ";
			 _ ->
			     "   " ++ string:right("", 2) ++ "| "
		     end;
		 _ ->
		     case Type of
			 background when ActID =:= 1 ->
			     "Ex." ++ string:right(integer_to_list(EID), 2) ++ "| ";
			 _ ->
			     "   " ++ string:right("", 2) ++ "| "
		     end
	     end
    end.

print_full_stats(St) ->
    ScnFailed = length(ets:match(kctx, {{'$1', status, '$2', '$3'}, failed})),
    ScnPassed = length(ets:match(kctx, {{'$1', status, '$2', '$3'}, ok})),
    StepsFailed = length(ets:match(kctx, {{'$1', '$2', '$3', '$4', '$5'}, {failed, '_'}})),
    StepsPassed = length(ets:match(kctx, {{'$1', '$2', '$3', '$4', '$5'}, ok})),
    StepsSkipped = length(ets:match(kctx, {{'$1', '$2', '$3', '$4', '$5'}, disabled})),
    StepsNI = length(ets:match(kctx, {{'$1', '$2', '$3', '$4', '$5'}, not_implementated})),

    io:format("~p Scenario (~s, ~s)~n",
	      [ScnFailed+ScnPassed,
	       stats_failed_str(St, ScnFailed),
	       stats_ok_str(St, ScnPassed)]),

    io:format("~p Steps (~s, ~s, ~s, ~s)~n",
	      [StepsFailed + StepsPassed + StepsSkipped + StepsNI,
	       stats_failed_str(St, StepsFailed),
	       stats_ok_str(St, StepsPassed),
	       stats_disabled_str(St, StepsSkipped),
	       stats_ni_str(St, StepsNI)
	      ]).



esc_helper(save_cursor) -> "\e[s";
esc_helper(restore_cursor) -> "\e[u";
esc_helper(erase_line) -> "\e[2K";
esc_helper(_) -> "".

tab_level(0) -> "";
tab_level(1) -> "  ";
tab_level(2) -> "    ";
tab_level(3) -> "      ";
tab_level(_) -> "".

background_str() -> "[BG] ".

ok_str(St) -> case St#state.color of true -> "\e[32mOK\e[0m"; _ -> "OK" end.
disabled_str(St) -> case St#state.color of true -> "\e[34mSkipped\e[0m"; _ -> "Skipped" end.
not_impl_str(St) -> case St#state.color of true -> "\e[35mNot implementated\e[0m"; _ -> "Not implementated" end.
failed_str(St) -> case St#state.color of true -> "\e[31mFAILED\e[0m"; _ -> "FAILED" end.

stats_failed_str(St,Num) -> case St#state.color of
			     true -> "\e[31m" ++ integer_to_list(Num) ++" failed"++"\e[0m";
			     _ ->  integer_to_list(Num) ++" failed"
			 end.
stats_ok_str(St,Num) ->
    case St#state.color of
	true -> "\e[32m" ++ integer_to_list(Num) ++" passed"++"\e[0m";
	_ ->  integer_to_list(Num) ++" passed"
    end.
stats_disabled_str(St,Num) ->
    case St#state.color of
	true -> "\e[34m" ++ integer_to_list(Num) ++" skipped"++"\e[0m";
	_ ->  integer_to_list(Num) ++" skipped"
    end.
stats_ni_str(St,Num) ->
    case St#state.color of
	true -> "\e[35m" ++ integer_to_list(Num) ++" not implementated"++"\e[0m";
	_ ->  integer_to_list(Num) ++" not implementated"
    end.

step2str(Type) ->
    case Type of
	given_step -> "Given";
	when_step -> "When";
	then_step -> "Then";
	and_step ->  "And"
    end.

