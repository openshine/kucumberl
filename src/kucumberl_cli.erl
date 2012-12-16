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
%%% Created : 30 Oct 2012 by Roberto Majadas <roberto.majadas@openshine.com>
%%%-------------------------------------------------------------------
-module(kucumberl_cli).
-include("kucumberl.hrl").

%% API
-export([main/1]).

-record(conf, {verbose = false,
	       path_a = [],
	       path_z = [],
	       tags = [],
	       skip = [],
	       rskip = [],
	       fdir = [],
	       task = runtests,
	       features = [],
	       color = true
	      }).

%%%===================================================================
%%% API
%%%===================================================================


%%main([]) -> usage();
main(Args) ->
    ets:new(kctx, [set, named_table, public]),
    kucumberl_log:start_link(),

    OptSpecList = option_spec_list(),

    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, _NonOptArgs}} ->
	    Conf = store_conf(#conf{}, Options),

	    %% Setup Enviroment
	    code:add_pathsa(Conf#conf.path_a),
	    code:add_pathsz(Conf#conf.path_z),

	    kucumberl_log:set_color(Conf#conf.color),

	    Result =
		case Conf#conf.task of
		    help ->
			usage(OptSpecList),
			ok;
		    list ->
			task(list, Conf);
		    runtests ->
			task(runtests, Conf);
		    _ -> ignoreit
		end,
	    case Result of
		error ->
		    kucumberl_utils:exit(1);
		_ ->
		    ignoreit
	    end;
        {error, {_Reason, _Data}} ->
            usage(OptSpecList)
    end,
    ok.

%%===================================================================
%% Internal functions
%%===================================================================

usage(OptSpecList) ->
    getopt:usage(OptSpecList,
		 "kucumberl",
		 "",
                 []).

option_spec_list() ->
    [
     %% {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
     {help,        $h,        "help",        undefined,             "Show the program options"},
     {verbose,     $v,        undefined,     boolean,               "Be verbose about what gets done"},
     {fdir,        $d,        undefined,     string,                "Feature's directory"},
     {list,        $l,        "list",        undefined,             "List features"},
     {tags,        $t,        "tags",        {string, ""},          "Only execute the features or scenarios with tags matching"},
     {skip,        $s,        "skip",        string,                "Skip feature"},
     {rskip,       $r,        "rskip",       string,                "Skip features using regexp"},
     {path_a,      $a,        "pa",          string,                "Adds the specified directories to the beginning of the code path"},
     {path_z,      $z,        "pz",          string,                "Adds the specified directories to the end of the code path"},
     {color,       $C,        "color",       {boolean, true},       "Use colors or not (enabled by default)"}
    ].


store_conf(Conf, [I|Rest]) ->
    case I of
	help         -> NewConf = Conf#conf{task=help};
	list         -> NewConf = Conf#conf{task=list};
	verbose      -> NewConf = Conf#conf{verbose=true};
	{tags, S}    -> NewConf = Conf#conf{tags = Conf#conf.tags ++ [string:tokens(S, ",")]};
	{skip, S}    -> NewConf = Conf#conf{skip = Conf#conf.skip ++ [S ++ "$"]};
	{rskip, S}   -> NewConf = Conf#conf{rskip = Conf#conf.rskip ++ [S]};
	{path_a, S}  -> NewConf = Conf#conf{path_a = Conf#conf.path_a ++ [S]};
	{path_z, S}  -> NewConf = Conf#conf{path_z = Conf#conf.path_z ++ [S]};
	{fdir, S}    -> NewConf = Conf#conf{fdir = S};
	{color, B}   -> NewConf = case B of
				      true -> Conf#conf{color = true};
				      false -> Conf#conf{color = false}
				  end;
	_ -> NewConf = Conf
    end,
    store_conf(NewConf, Rest);
store_conf(Conf, []) ->
    Conf1 = case Conf#conf.fdir of
		[] ->
		    {_, Cwd} = file:get_cwd(),
		    Conf#conf{fdir = Cwd};
		_ -> Conf
	    end,
    FeatureFiles = filelib:fold_files(Conf1#conf.fdir,
				      ".*\\.feature\$",
				      true,
				      fun(F, Acc) ->
					      [F | Acc]
				      end,
				      []),
    store_features(Conf1, 1, lists:sort(FeatureFiles)).

store_features(Conf, ID, [File|R]) ->
    case skip_feature(Conf, File) of
	yes ->
	    io:format("SKIP ~s~n", [re:replace(File,
				      "^" ++ Conf#conf.fdir,
				      "")]),
	    NextID = ID,
	    NewConf = Conf;
	no  ->
	    case kucumberl_parser:parse(File) of
		{ok, F} ->
		    F1 = F#feature{id = ID},
		    NextID = ID + 1,
		    NewConf = Conf#conf{features = Conf#conf.features ++ [F1]};
		{error, Reason} ->
		    EFile =re:replace(File,
				      "^" ++ Conf#conf.fdir,
				      ""),
		    io:format("~s:~n  ~s~n", [EFile, Reason]),
		    NextID = ID,
		    NewConf = Conf
	    end
    end,
    store_features(NewConf, NextID, R);
store_features(Conf, _, []) -> Conf.

skip_feature(Conf, F) ->
    S = lists:foldl(
	  fun (I, L) ->
		  case re:run(F, I, [global, unicode]) of
		      {match, _} -> L ++ match;
		      _ -> L
		  end
	  end, [], Conf#conf.skip ++ Conf#conf.rskip),
    case S of
	[] -> no;
	_ -> yes
    end.

task(list, Conf) ->
    PrintFeature =
	fun (F) ->
		io:format("~s~n",
			  [
			   re:replace(F#feature.path,
				      "^" ++ Conf#conf.fdir,
				      "")
			  ]),
		ok
	end,
    lists:foreach (PrintFeature, Conf#conf.features),
    ok;

task(runtests, Conf) ->
    Features = lists:foldl(
    		 fun(F, L) ->
			 case kucumberl_feature_code:setup(F) of
			     {ok, NF} -> L ++ [NF];
			     _ -> L
			 end
    		 end, [], Conf#conf.features),
    case kucumberl_feature_code:check_errors(Features) of
	ok ->
	    lists:foldl(fun(F, L) -> L ++ [kucumberl_feature:run(F)] end,
			[],
			Features),
	    kucumberl_log:print_stats(),
	    
	    ScnFailed = length(ets:match(kctx, {{'$1', status, '$2', '$3'}, failed})),
	    case ScnFailed of
		0 -> ok;
		_ -> error
	    end;
	error -> error
    end.

