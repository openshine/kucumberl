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

parse (Files) ->
    lists:foldl(
      fun (File, FList) ->
	      case parse_feature(File) of
		  {ok, F} -> io:format("~p~n", [F]), FList ++ [F];
		  {error, Reason} -> io:format("~s~n", [Reason]), FList
	      end
      end
      , [], Files).

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_feature(File) ->
    F = #feature{path=File},
    Ctx = #fparser_ctx{result=F},

    case file:read_file(File) of
	{ok, FileContent} ->
	    Lines = re:split(FileContent, "\r\n|\n|\r|\032", [{return, list}]),
	    case parse_lines(Ctx, Lines) of
		{error, Reason} -> {error, Reason};
		Result -> {ok, Result}
	    end;
	{error, Reason} ->
	    io:format("Error reading ~p: '~p'~n", [File, Reason]),
	    {error, Reason}
    end.


parse_lines({error, Reason}, _Rest) -> {error, Reason};

parse_lines(Ctx, [Line|Rest]) ->
    Ctx1 = Ctx#fparser_ctx{line = Ctx#fparser_ctx.line + 1},
    PCtx = case process_line(Line) of
	       {feature,    Info} ->
		   process_stage(feature, Ctx1, Info);
	       {background, Info} ->
		   process_stage(background, Ctx1, Info);
	       {scenario,   Info} ->
		   process_stage(scenario, Ctx1, Info);
	       {given_step, Info} ->
		   process_stage({step, given_step}, Ctx1, Info);
	       {when_step,  Info} ->
		   process_stage({step, when_step}, Ctx1, Info);
	       {then_step,  Info} ->
		   process_stage({step, then_step}, Ctx1, Info);
	       {and_step,   Info} ->
		   process_stage({step, and_step}, Ctx1, Info);
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
			gwta_step,
			comment]).

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
match_line(gwta_step, Line) ->
    case re:run(Line,
		"^\s+(?<STEP>(Given|When|Then|And))\s+(?<VALUE>.*)$",
		[global,{capture, [1,3], list},unicode]) of
	{match,[[STEP, VALUE]]} -> {ok, {list_to_atom(string:to_lower(STEP ++ "_step")),
					 VALUE}};
	_ -> pass
    end;

match_line(comment, Line) ->
    case re:run(Line, "^#.*$") of
	{match, _} -> {ok, {comment}};
	_ -> pass
    end;
match_line(_, _Line) -> pass.

format_perror(Ctx, Str, Data) ->
    lists:flatten(io_lib:format("[Error:~p] " ++ Str,
				[Ctx#fparser_ctx.line] ++ Data)).


scope_push(Ctx, Level) ->
    Ctx#fparser_ctx{scope = Ctx#fparser_ctx.scope ++ [Level]}.
scope_set(Ctx, NewScope) ->
    Ctx#fparser_ctx{scope = NewScope}.
%% scope_pop (Ctx) ->
%%     Ctx#fparser_ctx{scope = lists:reverse(tl(lists:reverse(Ctx#fparser_ctx.scope)))}.
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
process_stage({step, S}, Ctx, Info) ->
    case scope_get(Ctx) of
	[feature, background|R] -> store_step({background, S, R}, Ctx, Info);
	[feature, scenario|R] -> store_step({scenario, S, R}, Ctx, Info);
	_ -> {error, format_perror(Ctx, "Can't use step's here", [])}
    end;
process_stage(_, Ctx, _Info) ->
    Ctx.

store_step({_, and_step, []}, Ctx, _Info) ->
    {error, format_perror(Ctx, "Can't use 'and' step here", [])};
store_step({background, S, _R}, Ctx,  Info) ->
    Ctx1 = scope_set(Ctx, [feature, background, S]),
    Actions = Ctx1#fparser_ctx.result#feature.background#scenario.actions,
    NewActions = Actions ++ [[S, Info, Ctx1#fparser_ctx.line]],
    NewBackground = Ctx1#fparser_ctx.result#feature.background#scenario{actions=NewActions},
    NewResult = Ctx1#fparser_ctx.result#feature{background = NewBackground},
    Ctx1#fparser_ctx{result = NewResult};

store_step({scenario, S, _R}, Ctx, Info) ->
    Ctx1 = scope_set(Ctx, [feature, scenario, S]),
    [Scenario|SRest] = lists:reverse(Ctx1#fparser_ctx.result#feature.scenarios),
    Actions = Scenario#scenario.actions,
    NewActions = Actions ++ [[S, Info, Ctx1#fparser_ctx.line]],
    NewScenario = Scenario#scenario{actions = NewActions},
    NewResult = Ctx1#fparser_ctx.result#feature{scenarios = lists:reverse([NewScenario] ++ SRest)},
    Ctx1#fparser_ctx{result = NewResult}.




