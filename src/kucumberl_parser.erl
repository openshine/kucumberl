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
%%% Created : 31 Oct 2012 by Roberto Majadas <roberto.majadas@openshine.com>
%%%-------------------------------------------------------------------
-module(kucumberl_parser).
-include("kucumberl.hrl").

%% API
-export([parse/1]).

-record(fparser_ctx, {line = 0, scope = [], result}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
parse(File) ->
    F = #feature{path=File},
    Ctx = #fparser_ctx{result=F},

    case file:read_file(File) of
	{ok, FileContent} ->
	    Lines = re:split(FileContent, "\r\n|\n|\r|\032", [{return, list}]),
	    case parse_lines(Ctx, Lines) of
		{error, Reason} -> {error, Reason};
		Result -> {ok, Result#fparser_ctx.result}
	    end;
	{error, Reason} ->
	    io:format("Error reading ~p: '~p'~n", [File, Reason]),
	    {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_lines({error, Reason}, _Rest) -> {error, Reason};

parse_lines(Ctx, [Line|Rest]) ->
    Ctx1 = Ctx#fparser_ctx{line = Ctx#fparser_ctx.line + 1},
    PCtx = case process_line(Line) of
	       {feature,      Info} ->
		   process_stage(feature, Ctx1, Info);
	       {background,   Info} ->
		   process_stage(background, Ctx1, Info);
	       {scenario,     Info} ->
		   process_stage(scenario, Ctx1, Info);
	       {scenario_out, Info} ->
		   process_stage(scenario_out, Ctx1, Info);
	       {so_example,   Info} ->
		   process_stage(so_example, Ctx1, Info);
	       {given_step,   Info} ->
		   process_stage({step, given_step}, Ctx1, Info);
	       {when_step,    Info} ->
		   process_stage({step, when_step}, Ctx1, Info);
	       {then_step,    Info} ->
		   process_stage({step, then_step}, Ctx1, Info);
	       {and_step,     Info} ->
		   process_stage({step, and_step}, Ctx1, Info);
	       {table_row,    Info} ->
		   process_stage(table_row, Ctx1, Info);
	       {ml_text,      Info} ->
		   process_stage(ml_text, Ctx1, Info);
	       {str,          Info} ->
		   process_stage(str, Ctx1, Info);
	       {comment} ->
		   Ctx1;
	       _ ->
		   Ctx1
    end,
    %%io:format("-> ~p~n", [PCtx]),
    parse_lines(PCtx, Rest);
parse_lines(Ctx, []) -> Ctx.

process_line(Line) ->
    process_line(Line, [feature,
			background,
			scenario,
			scenario_out,
			so_example,
			gwta_step,
			table_row,
			ml_text,
			comment,
			str
		        ]).

process_line(Line, [MatchF|RestF]) ->
    case match_line(MatchF, Line) of
	{ok, Result} -> Result;
	_ -> process_line(Line, RestF)
    end;
process_line(_Line, [])->
    {error, not_match}.


match_line(feature, Line) ->
    case re:run(Line,
		"^Feature:\s*(|(?<VALUE>.*))$",
		[global,{capture, [1], list},unicode]) of
	{match,[[[]]]} -> {ok, {feature, ''}};
	{match,[[VALUE]]} -> {ok, {feature, VALUE}};
	_ -> pass
    end;
match_line(background, Line) ->
    case re:run(Line,
		"^\s+Background:\s*(|(?<VALUE>.*))$",
		[global,{capture, [1], list},unicode]) of
	{match,[[[]]]} -> {ok, {background, ''}};
	{match,[[VALUE]]} -> {ok, {background, VALUE}};
	_ -> pass
    end;
match_line(scenario, Line) ->
    case re:run(Line,
		"^\s+Scenario:\s*(|(?<VALUE>.*))$",
		[global,{capture, [1], list},unicode]) of
	{match,[[[]]]} -> {ok, {scenario, ''}};
	{match,[[VALUE]]} -> {ok, {scenario, VALUE}};
	_ -> pass
    end;
match_line(scenario_out, Line) ->
    case re:run(Line,
		"^\s+Scenario\s+Outline:\s*(|(?<VALUE>.*))$",
		[global,{capture, [1], list},unicode]) of
	{match,[[[]]]} -> {ok, {scenario_out, ''}};
	{match,[[VALUE]]} -> {ok, {scenario_out, VALUE}};
	_ -> pass
    end;
match_line(so_example, Line) ->
    case re:run(Line,
		"^\s+Examples:(|(?<VALUE>.*))$",
		[global,{capture, [1], list},unicode]) of
	{match,[[[]]]} -> {ok, {so_example, ''}};
	{match,[[_]]} -> {ok, {so_example, ''}};
	_ -> pass
    end;
match_line(gwta_step, Line) ->
    case re:run(Line,
		"^\s+(?<STEP>(Given|When|Then|And))\s+(?<VALUE>.*)$",
		[global,{capture, [1,3], list},unicode]) of
	{match,[[STEP, VALUE]]} -> {ok, {list_to_atom(string:to_lower(STEP ++ "_step")),
					 VALUE}};
	_ -> pass
    end;
match_line(table_row, Line) ->
    case re:run(Line,
		"^\s+\\|(?<VALUE>.+)\\|\s*$",
		[global,{capture, [1], list},unicode]) of
	{match,[[[]]]} -> {ok, {table_row, ''}};
	{match,[[VALUE]]} -> {ok, {table_row, VALUE}};
	_ -> pass
    end;
match_line(ml_text, Line) ->
    case re:run(Line,
		"^(?<VALUE>\s+)\"\"\"\s*$",
		[global,{capture, [1], list},unicode]) of
	{match,[[VALUE]]} -> {ok, {ml_text, VALUE}};
	_ -> pass
    end;
match_line(comment, Line) ->
    case re:run(Line, "^#.*$") of
	{match, _} -> {ok, {comment}};
	_ -> pass
    end;
match_line(str, Line) ->
    {ok, {str,Line}};
match_line(_, _Line) -> pass.

format_perror(Ctx, Str, Data) ->
    lists:flatten(io_lib:format("[Error:~p] " ++ Str,
				[Ctx#fparser_ctx.line] ++ Data)).


scope_push(Ctx, Level) ->
    Ctx#fparser_ctx{scope = Ctx#fparser_ctx.scope ++ [Level]}.
scope_set(Ctx, NewScope) ->
    Ctx#fparser_ctx{scope = NewScope}.
scope_pop (Ctx) ->
    Ctx#fparser_ctx{scope = lists:reverse(tl(lists:reverse(Ctx#fparser_ctx.scope)))}.
scope_get (Ctx) -> Ctx#fparser_ctx.scope.
scope_get_last (Ctx) ->
    case lists:reverse(scope_get(Ctx)) of
	[] -> [];
	[Item|_] -> Item
    end.

process_stage(feature, Ctx, Info) ->
    case scope_get(Ctx) of
	[] ->
	    Ctx1 = scope_push(Ctx, feature),
	    Ctx1#fparser_ctx{result = Ctx1#fparser_ctx.result#feature{desc = Info}};
	_  ->
	    {error, format_perror(Ctx, "Can't use 'feature' keyword here", [])}
    end;
process_stage(background, Ctx, Info) ->
    case scope_get_last(Ctx) of
	feature ->
	    Ctx1 = scope_push(Ctx, background),
	    case Ctx1#fparser_ctx.result#feature.background of
		[] ->
		    BS = #scenario{type=background,
				   desc = Info},
		    Ctx1#fparser_ctx{result = Ctx1#fparser_ctx.result#feature{background = BS}};
		_-> {error, format_perror(Ctx1, "Only one 'background' is mandatory", [])}
	    end;
	_ ->
	    {error, format_perror(Ctx, "Can't use 'background' keyword here", [])}
    end;
process_stage(scenario, Ctx, Info) ->
    case scope_get(Ctx) of
	[feature|_] ->
	    Ctx1 = scope_set(Ctx, [feature, scenario]),
	    Scenario = #scenario{type=scenario,
				 desc = Info},
	    NewSceneriosList = Ctx1#fparser_ctx.result#feature.scenarios ++ [Scenario],
	    Ctx1#fparser_ctx{result = Ctx1#fparser_ctx.result#feature{scenarios=NewSceneriosList}};
	_  ->
	    {error, format_perror(Ctx, "Can't use 'scenario' keyword here", [])}
    end;
process_stage(scenario_out, Ctx, Info) ->
    case scope_get(Ctx) of
	[feature|_] ->
	    Ctx1 = scope_set(Ctx, [feature, scenario_out]),
	    Scenario = #scenario{type=scenario_out,
				 desc = Info},
	    NewSceneriosList = Ctx1#fparser_ctx.result#feature.scenarios ++ [Scenario],
	    Ctx1#fparser_ctx{result = Ctx1#fparser_ctx.result#feature{scenarios=NewSceneriosList}};
	_  ->
	    {error, format_perror(Ctx, "Can't use 'scenario Outline' keyword here", [])}
    end;
process_stage(so_example, Ctx, _Info) ->
    case scope_get(Ctx) of
	[feature, scenario_out] ->
	    {error, format_perror(Ctx, "Can't use 'Example' keyword here", [])};
	[feature, scenario_out|R] ->
	    case R of
		[] -> {error, format_perror(Ctx, "Can't use 'Example' keyword here", [])};
		[so_example] -> {error, format_perror(Ctx, "Can't use 'Example' keyword here", [])}; 
		_  ->
		    scope_set(Ctx, [feature, scenario_out, so_example])
	    end;
	_ ->
	    {error, format_perror(Ctx, "Can't use 'Example' keyword here", [])}
    end;

process_stage({step, S}, Ctx, Info) ->
    case scope_get(Ctx) of
	[feature, background|R] -> store_step({background, S, R}, Ctx, Info);
	[feature, scenario|R] -> store_step({scenario, S, R}, Ctx, Info);
	[feature, scenario_out|R] -> store_step({scenario_out, S, R}, Ctx, Info);
	_ -> {error, format_perror(Ctx, "Can't use step's here", [])}
    end;
process_stage(table_row, Ctx, Info) ->
    case scope_get(Ctx) of
	[feature, scenario_out, so_example] ->
	    RowCells = string:tokens(Info, "|"),
	    StripFunc =
		fun (I, L) ->
			L ++ [string:strip(I, both)]
		end,
	    RowCells1 = lists:foldl(StripFunc, [], RowCells),
	    [Scenario|SRest] = lists:reverse(Ctx#fparser_ctx.result#feature.scenarios),
	    Examples = Scenario#scenario.examples,
	    NewExamples = Examples ++ [RowCells1],
	    NewScenario = Scenario#scenario{examples = NewExamples},
	    NewResult = Ctx#fparser_ctx.result#feature{scenarios = lists:reverse([NewScenario] ++ SRest)},
	    Ctx#fparser_ctx{result = NewResult};
	[feature, Scn, S]
	  when S =:= given_step;
	       S =:= when_step;
	       S =:= then_step;
	       S =:= and_step ->
	    RowCells = string:tokens(Info, "|"),
	    StripFunc =
		fun (I, L) ->
			L ++ [string:strip(I, both)]
		end,
	    RowCells1 = lists:foldl(StripFunc, [], RowCells),
	    case Scn of
		X when X =:= scenario;
		       X =:= scenario_out ->
		    [Scenario|SRest] = lists:reverse(Ctx#fparser_ctx.result#feature.scenarios),
		    [Action|ARest] = lists:reverse(Scenario#scenario.actions),
		    NewTableTxt = case Action#action.tabletxt of
				      "" -> Info;
				      _ -> Action#action.tabletxt ++ "\n" ++ Info
				  end,
		    NewAction = Action#action{table = Action#action.table ++ [RowCells1],
					      tabletxt = NewTableTxt
					     },
		    NewScenario = Scenario#scenario{actions = lists:reverse([NewAction] ++ ARest)},
		    NewResult = Ctx#fparser_ctx.result#feature{scenarios = lists:reverse([NewScenario] ++ SRest)},
		    Ctx#fparser_ctx{result = NewResult};
		background ->
		    Scenario = Ctx#fparser_ctx.result#feature.background,
		    [Action|ARest] = lists:reverse(Scenario#scenario.actions),
		    NewAction = Action#action{table = Action#action.table ++ [RowCells1]},
		    NewScenario = Scenario#scenario{actions = lists:reverse([NewAction] ++ ARest)},
		    NewResult = Ctx#fparser_ctx.result#feature{background = NewScenario},
		    Ctx#fparser_ctx{result = NewResult}
	    end;
	_ ->
	    {error, format_perror(Ctx, "Can't use table row here", [])}
    end;
process_stage(ml_text, Ctx, Info) ->
    case scope_get_last(Ctx) of
	S when S =:= given_step;
	       S =:= when_step;
	       S =:= then_step;
	       S =:= and_step ->
	    scope_push(Ctx, [ml_text, Info]);

	[ml_text, _] ->
	    scope_pop(Ctx)
    end;
process_stage(str, Ctx, Info) ->
    case scope_get(Ctx) of
	[feature, Scn, S, [ml_text, T]]
	  when S =:= given_step;
	       S =:= when_step;
	       S =:= then_step;
	       S =:= and_step ->
	    Str = case re:run(Info,
			      "^" ++ T ++ "(?<VALUE>.*)$",
			      [global,{capture, [1], list},unicode]) of
		      {match,[[VALUE]]} -> VALUE;
		      _ -> Info
		  end,
	    case Scn of
		background ->
		    Scenario = Ctx#fparser_ctx.result#feature.background,
		    [Action|ARest] = lists:reverse(Scenario#scenario.actions),
		    NewAction = case Action#action.text of
				    "" -> Action#action{text = Str};
				    _  -> Action#action{text = Action#action.text ++ "\n" ++ Str}
				end,
		    NewScenario = Scenario#scenario{actions = lists:reverse([NewAction] ++ ARest)},
		    NewResult = Ctx#fparser_ctx.result#feature{background = NewScenario},
		    Ctx#fparser_ctx{result = NewResult};
		X when X =:= scenario;
		       X =:= scenario_out->
		    [Scenario|SRest] = lists:reverse(Ctx#fparser_ctx.result#feature.scenarios),
		    [Action|ARest] = lists:reverse(Scenario#scenario.actions),
		    NewAction = case Action#action.text of
				    "" -> Action#action{text = Str};
				    _  -> Action#action{text = Action#action.text ++ "\n" ++ Str}
				end,
		    NewScenario = Scenario#scenario{actions = lists:reverse([NewAction] ++ ARest)},
		    NewResult = Ctx#fparser_ctx.result#feature{scenarios = lists:reverse([NewScenario] ++ SRest)},
		    Ctx#fparser_ctx{result = NewResult}
	    end;
	_ -> Ctx
    end;
process_stage(_, Ctx, _Info) ->
    Ctx.

store_step({_, and_step, []}, Ctx, _Info) ->
    {error, format_perror(Ctx, "Can't use 'and' step here", [])};

store_step({background, S, _R}, Ctx,  Info) ->
    Ctx1 = scope_set(Ctx, [feature, background, S]),
    Actions = Ctx1#fparser_ctx.result#feature.background#scenario.actions,
    NewActions = Actions ++ [#action{step=S, desc=Info, line=Ctx1#fparser_ctx.line}],
    NewBackground = Ctx1#fparser_ctx.result#feature.background#scenario{actions=NewActions},
    NewResult = Ctx1#fparser_ctx.result#feature{background = NewBackground},
    Ctx1#fparser_ctx{result = NewResult};

store_step({scenario, S, _R}, Ctx, Info) ->
    Ctx1 = scope_set(Ctx, [feature, scenario, S]),
    [Scenario|SRest] = lists:reverse(Ctx1#fparser_ctx.result#feature.scenarios),
    Actions = Scenario#scenario.actions,
    NewActions = Actions ++ [#action{step=S, desc=Info, line=Ctx1#fparser_ctx.line}],
    NewScenario = Scenario#scenario{actions = NewActions},
    NewResult = Ctx1#fparser_ctx.result#feature{scenarios = lists:reverse([NewScenario] ++ SRest)},
    Ctx1#fparser_ctx{result = NewResult};

store_step({scenario_out, S, _R}, Ctx, Info) ->
    Ctx1 = scope_set(Ctx, [feature, scenario_out, S]),
    [Scenario|SRest] = lists:reverse(Ctx1#fparser_ctx.result#feature.scenarios),
    Actions = Scenario#scenario.actions,
    NewActions = Actions ++ [#action{step=S, desc=Info, line=Ctx1#fparser_ctx.line}],
    NewScenario = Scenario#scenario{actions = NewActions},
    NewResult = Ctx1#fparser_ctx.result#feature{scenarios = lists:reverse([NewScenario] ++ SRest)},
    Ctx1#fparser_ctx{result = NewResult}.


