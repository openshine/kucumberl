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
	       skip = [],
	       rskip = [],
	       fdir = [],
	       task = runtests,
	       features = []
	      }).

%%%===================================================================
%%% API
%%%===================================================================


main([]) -> usage();
main(Args) ->
    OptSpecList = option_spec_list(),

    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, _NonOptArgs}} ->
	    Conf = store_conf(#conf{}, Options),

	    %% Setup Enviroment
	    code:add_pathsa(Conf#conf.path_a),
	    code:add_pathsz(Conf#conf.path_z),

	    case Conf#conf.task of
		help ->
		    usage(OptSpecList);
		list ->
		    task(list, Conf);
		runttest ->
		    task(runtests, Conf);
		_ -> ignoreit
	    end;
        {error, {_Reason, _Data}} ->
            usage(OptSpecList)
    end,
    ok.

%%===================================================================
%% Internal functions
%%===================================================================

usage() ->
    usage(option_spec_list()).

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
     {skip,        $s,        "skip",        string,                "Skip feature"},
     {rskip,       $r,        "rskip",       string,                "Skip features using regexp"},
     {path_a,      $a,        "pa",          string,                "Adds the specified directories to the beginning of the code path"},
     {path_z,      $z,        "pz",          string,                "Adds the specified directories to the end of the code path"}
    ].


store_conf(Conf, [I|Rest]) ->
    case I of
	help         -> NewConf = Conf#conf{task=help};
	list         -> NewConf = Conf#conf{task=list};
	verbose      -> NewConf = Conf#conf{verbose=true};
	{skip, S}    -> NewConf = Conf#conf{skip = Conf#conf.skip ++ [S ++ "$"]};
	{rskip, S}   -> NewConf = Conf#conf{rskip = Conf#conf.rskip ++ [S]};
	{path_a, S}  -> NewConf = Conf#conf{path_a = Conf#conf.path_a ++ [S]};
	{path_z, S}  -> NewConf = Conf#conf{path_z = Conf#conf.path_z ++ [S]};
	{fdir, S}    -> NewConf = Conf#conf{fdir = S};
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
    store_features(Conf1, FeatureFiles).

store_features(Conf, [File|R]) ->
    case skip_feature(Conf, File) of
	yes ->
	    io:format("SKIP ~s~n", [re:replace(File,
				      "^" ++ Conf#conf.fdir,
				      "")]),
	    NewConf = Conf;
	no  ->
	    case kucumberl_parser:parse(File) of
		{ok, F} ->
		    NewConf = Conf#conf{features = Conf#conf.features ++ [F]};
		{error, Reason} ->
		    EFile =re:replace(File,
				      "^" ++ Conf#conf.fdir,
				      ""),
		    io:format("~s:~n  ~s~n", [EFile, Reason]),
		    NewConf = Conf
	    end
    end,
    store_features(NewConf, R);
store_features(Conf, []) -> Conf.

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
    lists:foreach (PrintFeature, Conf#conf.features);

task(runtests, _Conf) ->
    ok.
