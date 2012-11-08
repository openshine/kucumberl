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
-record(feature_ctx, {feature,
		      mods,
		      step_re=[],
		      setup_mod = [],
		      teardown_mod = [],
		      scn_ctxs=[]
		     }).

-record(scn_ctx, {status = ok,
		  state = [],
		  step_type,
		  stats_steps = 0,
		  stats_steps_ok = 0,
		  stats_steps_not_implemented = 0,
		  stats_steps_disabled = 0,
		  stats_steps_failed = 0
		 }).
-record(code_mods, {ok = [], errors=false}).

%%%===================================================================
%%% API
%%%===================================================================

run(Feature) ->
    Ctx = setup(#feature_ctx{feature = Feature,
			     mods = #code_mods{}
			    }),

    case Ctx#feature_ctx.mods#code_mods.errors of
	true -> clean(Ctx), {error, Ctx};
	false ->
	    log_feature(Ctx),
	    Ctx1 = run_feature(Ctx),
	    clean(Ctx1),
	    {ok, Ctx1}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

clean(Ctx) ->
    lists:foreach(fun (M) ->
			  code:delete(M),
			  code:purge(M)
		  end,
		  Ctx#feature_ctx.mods#code_mods.ok).

relative_path(P) ->
    {_, Cwd} = file:get_cwd(),
    re:replace(P, "^" ++ Cwd ++ ".",
	       "", [{return, list}]).

get_lib_files(Ctx) ->
    FFile = Ctx#feature_ctx.feature#feature.path,
    LibDir = filename:join([filename:dirname(filename:dirname(FFile))]
			   ++ ["lib"]),
    filelib:fold_files(LibDir,
		       ".*\\.erl\$",
		       false,
		       fun(F, Acc) ->
			       R = relative_path(F),
			       [R | Acc]
		       end,
		       []).

get_steps_files(Ctx) ->
    FFile = Ctx#feature_ctx.feature#feature.path,
    StepDir = filename:join([filename:dirname(FFile)] ++ ["step_definitions"]),

    filelib:fold_files(StepDir,
		       ".*\\.erl\$",
		       false,
		       fun(F, Acc) ->
			       R = relative_path(F),
			       [R | Acc]
				 end,
		       []).

load_code_files(Ctx) ->
    CodeFiles = get_lib_files(Ctx) ++ get_steps_files(Ctx),

    CF = fun(File, C) ->
		 CRet = compile:file(File,
				     [binary, return, report]),
		 case CRet of
		     {ok, ModuleName,Binary} ->
			 code:load_binary(ModuleName, "generated", Binary),
			 M = C#feature_ctx.mods#code_mods{ok = C#feature_ctx.mods#code_mods.ok ++
								[ModuleName]},
			 Ctx#feature_ctx{mods = M};
		     {ok,ModuleName,Binary,Warnings} ->
			 case Warnings of
			     [] ->
				 code:load_binary(ModuleName, "generated", Binary),
				 M = C#feature_ctx.mods#code_mods{ok = C#feature_ctx.mods#code_mods.ok ++
								      [ModuleName]},
				 C#feature_ctx{mods = M};
			     _ ->
				 M = C#feature_ctx.mods#code_mods{errors=true},
				 C#feature_ctx{mods = M}
			 end;
		     {error,_Errors,_Warnings} ->
			 M = C#feature_ctx.mods#code_mods{errors=true},
			 C#feature_ctx{mods = M}
		 end
	 end,
    lists:foldl(CF, Ctx, CodeFiles).


extract_regexp_from_stepfile(StepModule, Ctx, Form) ->
    F = fun (I, C) ->
		case I of
		    {function,_,S, 3, Clauses}
		      when S =:= given;
			   S =:= 'when';
			   S =:= then ->

			StoreRe =
			    fun (Sx, CC, R) ->
				    CC#feature_ctx{step_re = CC#feature_ctx.step_re
						   ++ [{Sx, StepModule, R}]}
			    end,

			ClauseFunc =
			    fun (Clause, CC) ->
				    case Clause of
				    	{clause, _, [{string,_,R}|_], _, _} ->
				    	    case S of
				    		given ->
				    		    StoreRe(given_step, CC, R);
				    		'when' ->
				    		    StoreRe(when_step, CC, R);
				    		then ->
				    		    StoreRe(then_step, CC, R)
				    	    end
				    end
			    end,
			lists:foldl(ClauseFunc, C, Clauses);
		    _ -> C
		end
	end,

    lists:foldl(F, Ctx, Form).

load_step_regexp(Ctx) ->
    FFile = Ctx#feature_ctx.feature#feature.path,
    StepModule = list_to_atom(re:replace(filename:basename(FFile),
					 ".feature$", "",
					 [{return, list}])),
    StepDir = filename:join([filename:dirname(FFile)] ++ ["step_definitions"]),
    StepFile = filename:join([StepDir] ++ [re:replace(filename:basename(FFile),
						      ".feature$", ".erl",
						      [{return, list}])]),
    case epp:parse_file(StepFile, "", "") of
	{ok, Form} ->
	    extract_regexp_from_stepfile(StepModule, Ctx, Form);
	{error, _OpenError} ->
	    Ctx
    end.

extract_setup_teardown_funcs(StepModule, Ctx, Form) ->
    F = fun (I, C) ->
		case I of
		    {function,_,S,0,_} when S =:= setup; S =:= teardown ->
			case S of
			    setup -> C#feature_ctx{setup_mod = StepModule};
			    teardown -> C#feature_ctx{teardown_mod = StepModule}
			end;
		    _ -> C
		end
	end,

    lists:foldl(F, Ctx, Form).

set_setup_teardown_funcs(Ctx) ->
    FFile = Ctx#feature_ctx.feature#feature.path,
    StepModule = list_to_atom(re:replace(filename:basename(FFile),
					 ".feature$", "",
					 [{return, list}])),
    StepDir = filename:join([filename:dirname(FFile)] ++ ["step_definitions"]),
    StepFile = filename:join([StepDir] ++ [re:replace(filename:basename(FFile),
						      ".feature$", ".erl",
						      [{return, list}])]),
    case epp:parse_file(StepFile, "", "") of
	{ok, Form} ->
	    extract_setup_teardown_funcs(StepModule, Ctx, Form);
	{error, _OpenError} ->
	    Ctx
    end.

load_rest_step_regexp(Ctx) ->
    StepFiles = get_steps_files(Ctx),
    FFile = Ctx#feature_ctx.feature#feature.path,
    StepDir = filename:join([filename:dirname(FFile)] ++ ["step_definitions"]),
    StepFile = relative_path(filename:join([StepDir] ++ [re:replace(filename:basename(FFile),
								    ".feature$", ".erl",
								    [{return, list}])])),

    lists:foldl(
      fun (F, C) ->
	      StepModule = list_to_atom(re:replace(filename:basename(F),
						   ".erl$", "",
						   [{return, list}])),
	      case F of
		  F1 when F1 =:= StepFile -> C;
		  F2 ->
		      case epp:parse_file(F2, "", "") of
			  {ok, Form} ->
			      extract_regexp_from_stepfile(StepModule, C, Form);
			  {error, _OpenError} ->
			      C
		      end
	      end
      end, Ctx, StepFiles).

setup(Ctx) ->
    %% Load code from lib directory
    Ctx1 = load_code_files(Ctx),
    Ctx2 = load_step_regexp(Ctx1),
    Ctx3 = load_rest_step_regexp(Ctx2),
    set_setup_teardown_funcs(Ctx3).


run_feature(Ctx) ->
    lists:foldl(
      fun(Scn, C) ->
	      case Scn#scenario.type of
		  scenario ->
		      ScnCtx  = run_scenario_setup(C, #scn_ctx{}, Scn),
		      ScnCtx1 = run_scenario(C, ScnCtx, Scn),
		      ScnCtx2 = run_scenario_teardown(C, ScnCtx1, Scn),
		      io:format("~n"),
		      C#feature_ctx{scn_ctxs = C#feature_ctx.scn_ctxs ++ [ScnCtx2]};
		  scenario_out ->
		      run_scenario_outline(Scn,C)
	      end
      end,
      Ctx, Ctx#feature_ctx.feature#feature.scenarios).

run_scenario_setup(C, ScnCtx, _Scn) ->
    case C#feature_ctx.setup_mod of
	[] -> ScnCtx;
	Mod ->
	    State = Mod:setup(),
	    ScnCtx#scn_ctx{state = State}
    end.

run_scenario_teardown(C, ScnCtx, _Scn) ->
    case C#feature_ctx.teardown_mod of
	[] -> ScnCtx#scn_ctx{state = ok};
	Mod ->
	    State = Mod:teardown(),
	    ScnCtx#scn_ctx{state = State}
    end.

run_scenario(Ctx, ScnCtx, Scn) ->
    log_scenario(Scn),
    lists:foldl(fun(Act, SCtx) ->
			run_action(Ctx, SCtx, Act)
		end,
		ScnCtx,
		case Ctx#feature_ctx.feature#feature.background of
		    [] -> Scn#scenario.actions;
		    B -> B#scenario.actions ++ Scn#scenario.actions
		end).

prepare_action(Act, Elems) ->
    lists:foldl(
      fun ({K,V}, A) ->
	      NewDesc = re:replace(A#action.desc,
				   "<" ++ K ++ ">",
				   V,
				   [{return, list}]),
	      A#action{desc = NewDesc}
      end, Act, Elems).

prepare_scenario_template(Scn, Elems) ->
    NewActions =
	lists:foldl(
	  fun (Act, L) ->
		  L ++ [prepare_action(Act, Elems)]
	  end,
	  [], Scn#scenario.actions),
    Scn#scenario{actions = NewActions}.

run_scenario_outline_example(C, ScnTemplate, Elems) ->
    Scn = prepare_scenario_template(ScnTemplate, Elems),
    ScnCtx  = run_scenario_setup(C, #scn_ctx{}, Scn),
    ScnCtx1 = run_scenario(C, ScnCtx, Scn),
    ScnCtx2 = run_scenario_teardown(C, ScnCtx1, Scn),
    io:format("~n"),
    C#feature_ctx{scn_ctxs = C#feature_ctx.scn_ctxs ++ [ScnCtx2]}.

run_scenario_outline(Scn, C) ->
    case Scn#scenario.examples of
	[] -> C#feature_ctx{scn_ctxs = C#feature_ctx.scn_ctxs ++ [#scn_ctx{}]};
	[H|Rows] ->
	    lists:foldl(
	      fun(R, Ctx) ->
		      try lists:zip(H, R) of
			  Elems -> run_scenario_outline_example(Ctx, Scn, Elems)
		      catch
			  _ -> C#feature_ctx{scn_ctxs = C#feature_ctx.scn_ctxs ++ [#scn_ctx{}]}
		      end
	      end,
	      C, Rows)
    end.

run_action(Ctx, ScnCtx, Act) ->
    S = case Act#action.step of
	    and_step -> ScnCtx;
	    _ -> ScnCtx#scn_ctx{step_type = Act#action.step}
	end,

    log_action(Act),
    case check_step(Ctx, S, Act) of
	{implemented,Impl} ->
	    case S#scn_ctx.status of
		ok -> run_step(Ctx, S, Act, Impl);
		_  ->
		    log_action_disabled(),
		    S#scn_ctx{stats_steps = S#scn_ctx.stats_steps + 1,
			      stats_steps_disabled = S#scn_ctx.stats_steps_disabled + 1}
	    end;
	not_implemented ->
	    log_action_not_impl(),
	    S#scn_ctx{stats_steps = S#scn_ctx.stats_steps + 1,
		      stats_steps_not_implemented = S#scn_ctx.stats_steps_not_implemented + 1}
    end.

check_step(Ctx, ScnCtx, Act) ->
    StepRe = lists:foldl(fun (SRe, L) ->
				 case SRe of
				     {Step, StepModule, RegExp}
				       when Step =:= ScnCtx#scn_ctx.step_type ->
					 L ++ [{StepModule, RegExp}];
				     _ -> L
				 end
			 end,
			 [],
			 Ctx#feature_ctx.step_re),
    ActDesc = Act#action.desc,

    RSteps = lists:foldl(fun({SM, Re}, Result) ->
				 case re:run(ActDesc, Re) of
				     {match, _} -> Result ++ [{SM, Re}];
				     _ -> Result
				 end
			 end, [], StepRe),
    case RSteps of
	[S|_] -> {implemented, S};
	[] -> not_implemented
    end.

run_step(_Ctx, ScnCtx, Act, {Mod, Re}) ->
    P1 = case re:run(Act#action.desc, Re,
			 [{capture, all_but_first, list}]) of
		 {match, P} -> P;
		 _ -> []
	     end,
    P2 = case Act#action.text of
	     "" -> P1;
	     Text -> P1 ++ [Text]
	 end,
    Params = case Act#action.table of
		 [] -> P2;
		 Table -> P2 ++ [Table]
	     end,

    ExecStep =
    	fun(StepType) ->
    		case StepType of
    		    given_step -> Mod:given(Re, ScnCtx#scn_ctx.state, Params);
    		    when_step  -> Mod:'when'(Re, ScnCtx#scn_ctx.state, Params);
    		    then_step  -> Mod:then(Re, ScnCtx#scn_ctx.state, Params)
    		end
    	end,

    Result = try ExecStep(ScnCtx#scn_ctx.step_type) of
		 Val -> Val
	     catch
		 E -> {failed, E}
	     end,


    case Result of
	{ok, State} ->
	    log_action_ok(),
	    ScnCtx#scn_ctx{state = State,
			   stats_steps = ScnCtx#scn_ctx.stats_steps + 1,
			   stats_steps_ok = ScnCtx#scn_ctx.stats_steps_ok + 1
			  };
	{failed, Reason} ->
	    log_action_failed(Reason),
	    ScnCtx#scn_ctx{status=failed,
			   stats_steps = ScnCtx#scn_ctx.stats_steps + 1,
			   stats_steps_failed = ScnCtx#scn_ctx.stats_steps_failed + 1
			  }
    end.

log_feature(C) ->
    io:format("Feature: ~s~n", [C#feature_ctx.feature#feature.desc]).

log_scenario(Scn) ->
    io:format("  Scenario: ~s~n", [Scn#scenario.desc]).

log_action(Act) ->
    Step = case Act#action.step of
	       given_step -> "Given " ++ Act#action.desc;
	       when_step  -> "When " ++ Act#action.desc;
	       then_step  -> "Then " ++ Act#action.desc;
	       and_step   -> "And " ++ Act#action.desc
	   end,
    io:format("    ~s ", [string:left(Step, 60)]).

log_action_not_impl() -> io:format("Not implemented~n").
log_action_ok() -> io:format("OK~n").
log_action_failed(Reason) ->
    io:format("Fail!\n"),
    io:format("      \\_Reason: '~s'~n~n", [Reason]).
log_action_disabled() -> io:format("Disabled~n").
