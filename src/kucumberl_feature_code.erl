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
%%% Created :  9 Nov 2012 by Roberto Majadas <roberto.majadas@openshine.com>
%%%-------------------------------------------------------------------
-module(kucumberl_feature_code).
-include("kucumberl.hrl").

%% API
-export([setup/1, load/1, unload/1]).

%%%===================================================================
%%% API
%%%===================================================================

setup(Feature) ->
    case Feature#feature.fcode of
	[] ->
	    F = Feature#feature{fcode = #feature_code{}},
	    F1 = compile_code_files(F),
	    F2 = store_step_regexps(F1),
	    F3 = store_hook_funcs(F2),
	    {ok, F3};
	_ ->
	    {error, "The code is already loaded"}
    end.

load(Feature) ->
    Modules = Feature#feature.fcode#feature_code.modules,
    lists:foreach(
      fun (M) ->
	      case M#module.mod of
		  [] -> ignoreit;
		  [ModName, Bin] ->
		      code:load_binary(ModName, "generated", Bin)
	      end
      end, Modules).

unload(Feature) ->
    Modules = Feature#feature.fcode#feature_code.modules,
    lists:foreach(
      fun (M) ->
	      case M#module.mod of
		  [] -> ignoreit;
		  [ModName, _Bin] ->
		      case code:is_loaded(ModName) of
			  false -> ignoreit;
			  _ ->
			      code:delete(ModName),
			      code:purge(ModName)
		      end
	      end
      end, Modules).

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_code_files(F) ->
    Files = get_feature_files(F, lib_mods) ++ get_feature_files(F, step_mods),
    compile_code_files(F, Files).

compile_code_files(F, [File|Rest]) ->
    CRet = compile:file(File,
			[binary, return]),
    NewF = case CRet of
	       {ok, ModName, Bin} ->
		   OldModules = F#feature.fcode#feature_code.modules,
		   NewModules = OldModules ++ [#module{path=File, mod=[ModName, Bin]}],
		   FC = F#feature.fcode#feature_code{modules = NewModules},
		   F#feature{fcode = FC};
	       {ok, ModName, Bin, Warnings} ->
		   OldModules = F#feature.fcode#feature_code.modules,
		   NewModules = OldModules ++ [#module{path=File, mod=[ModName, Bin], warnings = Warnings}],
		   FC = F#feature.fcode#feature_code{modules = NewModules},
		   F#feature{fcode = FC};
	       {error, Errors, Warnings} ->
		   M = #module{path=File, mod=[], warnings = Warnings, errors = Errors},
		   FC = F#feature.fcode#feature_code{
				    status = found_errors,
				    modules = F#feature.fcode#feature_code.modules ++ [M]},
		   F#feature{fcode = FC}
	   end,
    compile_code_files(NewF, Rest);
compile_code_files(F, []) -> F.

store_step_regexps(F) ->
    DStep = get_default_step_file(F),
    Files = [DStep] ++ lists:delete(DStep, get_feature_files(F, step_mods)),
    store_step_regexps(F, Files).

store_step_regexps(F, [File|Rest]) ->
    NewF = case epp:parse_file(File, "", "") of
	       {ok, Form} ->
		   M = get_modname_from_erl(File),
		   extract_regexps(F, M, Form);
	       {error, _OpenError} ->
		   F
	   end,
    store_step_regexps(NewF, Rest);
store_step_regexps(F, []) -> F.

store_hook_funcs(F) ->
    DStep = get_default_step_file(F),
    StepModule = get_modname_from_erl(DStep),

    case epp:parse_file(DStep, "", "") of
	{ok, Form} ->
	    Func =
		fun (I, F1) ->
			case I of
			    {function,_,S,0,_}
			      when S =:= setup;
				   S =:= teardown ->
				case S of
				    setup ->
					FC = F1#feature.fcode#feature_code{setup_mod = StepModule},
					F1#feature{fcode = FC};
				    teardown ->
					FC = F1#feature.fcode#feature_code{teardown_mod = StepModule},
					F1#feature{fcode = FC}
				end;
			    _ -> F1
			end
		end,
	    lists:foldl(Func, F, Form);
	{error, _OpenError} ->
	    F
    end.

%%%===================================================================
%%% Util functions
%%%===================================================================

get_default_step_file(F) ->
    FFile = F#feature.path,
    StepDir = filename:join([filename:dirname(FFile)] ++ ["step_definitions"]),
    StepFile = filename:join([StepDir] ++ [re:replace(filename:basename(FFile),
						      ".feature$", ".erl",
						      [{return, list}])]),
    {_, Cwd} = file:get_cwd(),
    re:replace(StepFile, "^" ++ Cwd ++ ".",
	       "", [{return, list}]).

get_feature_files(F, Type) ->
    FFile = F#feature.path,
    FeatureRoot = filename:dirname(filename:dirname(FFile)),

    [TypePath, Ext] = case Type of
			  step_mods ->
			      [["features", "step_definitions"], ".erl"];
			  lib_mods ->
			      [["lib"], ".erl"]
		      end,

    Path = filename:join([FeatureRoot] ++ TypePath),

    filelib:fold_files(Path,
		       ".*\\" ++ Ext ++ "\$",
		       false,
		       fun(File, Acc) ->
			       {_, Cwd} = file:get_cwd(),
			       R = re:replace(File, "^" ++ Cwd ++ ".",
					  "", [{return, list}]),
			       [R | Acc]
				 end,
		       []).

get_modname_from_erl(File) ->
    list_to_atom(re:replace(filename:basename(File),
			    ".erl$", "",
			    [{return, list}])).

extract_regexps(F, M, [Form|Rest]) ->
    NewF = case Form of
	       {function,_,S, 3, Clauses}
		 when S =:= given;
		      S =:= 'when';
		      S =:= then ->

		   StoreRe =
		       fun (Sx, F1, R) ->
			       OldSteps = F1#feature.fcode#feature_code.steps,
			       NewSteps = OldSteps ++ [{Sx, M, R}],
			       FC = F1#feature.fcode#feature_code{steps = NewSteps},
			       F1#feature{fcode = FC}
		       end,

		   ClauseFunc =
		       fun (Clause, F1) ->
			       case Clause of
				   {clause, _, [{string,_,R}|_], _, _} ->
				       case S of
					   given ->
					       StoreRe(given_step, F1, R);
					   'when' ->
					       StoreRe(when_step,  F1, R);
					   then ->
					       StoreRe(then_step,  F1, R)
				       end
			       end
		       end,
		   lists:foldl(ClauseFunc, F, Clauses);
	       _ -> F
	   end,
    extract_regexps(NewF, M, Rest);
extract_regexps(F, _, []) -> F.

