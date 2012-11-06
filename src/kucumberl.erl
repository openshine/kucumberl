-module(kucumberl).

-include("kucumberl.hrl").

-export([main/1]).

main(Args) ->
    kucumberl_cli:main(Args).
