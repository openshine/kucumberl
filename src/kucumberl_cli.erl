%%%-------------------------------------------------------------------
%%% @author Roberto Majadas <roberto.majadas@openshine.com>
%%% @copyright (C) 2012, OpenShine S.L.
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2012 by Roberto Majadas <roberto.majadas@openshine.com>
%%%-------------------------------------------------------------------
-module(kucumberl_cli).

%% API
-export([main/1]).

%%%===================================================================
%%% API
%%%===================================================================

main(_Args) ->
    {_, Cwd} = file:get_cwd(),
    kucumberl_features:start(Cwd),
    ok.

%%===================================================================
%% Internal functions
%%===================================================================

