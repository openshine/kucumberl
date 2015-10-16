-module(editor).

-export([given/3, 'when'/3, then/3, setup/0, teardown/1]).

given ("a text like that:", _State, [Text]) ->
    {ok, Text};
given ("a users this table like that:", _State, [Table]) ->
    {ok, Table}.


'when' ("I replace '(\\w+)' with '(\\w+)'", State, [In, Out]) ->
    Text = re:replace(State, In, Out, [{return, list}]),
    {ok, Text};
'when' ("I press table2text", State, []) ->
    Text = lists:foldl(
	     fun ([Name, Org], Str) ->
		     Str ++ Name ++ Org ++ "\n"
	     end, "", State),
    {ok, Text}.

then ("the text will be like that:", State, [Text]) ->
    case Text == State of
	true -> {ok, Text};
	false -> {failed, "text not match"}
    end;
then ("i've a text like that:", State, [Text]) ->
    case Text == State of
	true -> {ok, Text};
	false -> {failed, "text not match"}
    end.


setup() ->
  [].

teardown(__State) ->
  ok.