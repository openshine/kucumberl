-module(fail).

-export([given/3, 'when'/3, then/3]).

given ("a step that works", State, []) ->
    {ok, State}.

'when' ("I come across a failing step", _State, []) ->
    {failed, "Fake failure! ;) "};
'when' ("this step throw an exception and fail", _State, []) ->
    throw("Boom"),
    {failed, "Fake failure! ;) "}.


then ("this step must be disabled", State, []) ->
    {ok, State};
then ("this one too", State, []) ->
    {ok, State}.


