-module(simple_sample).

-export([setup/0, teardown/0, given/3, 'when'/3, then/3]).


setup() -> [].

given ("I have entered (\\d+) into the calculator", State, [Num]) ->
    {ok, State ++ [Num]}.

'when' ("I press (\\w+)", State, [Op]) ->
    case Op of
	"add" -> {ok, calculator:add(State)};
	"multiply" -> {ok, calculator:multiply(State)}
    end.

then ("the result should be (\\d+) on the screen", State, [Num]) ->
    {I, _} = string:to_integer(Num),
    case State == I of
	true -> {ok, State};
	_ -> {failed, State}
    end.

teardown() -> ok.
