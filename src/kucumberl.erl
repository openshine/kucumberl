-module(kucumberl).

-include("kucumberl.hrl").

-export([main/1]).

main(_Args) ->
    kucumberl_cli:main(_Args).
