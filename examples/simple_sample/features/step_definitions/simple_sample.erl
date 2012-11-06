-module(simple_sample).

-export([given/3, 'when'/3, then/3]).

given ("I have entered (\d+) into the calculator", _State, [_Num]) ->
    ok.

'when' ("I press (\w+)", _State, [_Op]) ->
    ok.

then ("the result should be (\d+) on the screen", _State, [_Num]) ->
    ok.
