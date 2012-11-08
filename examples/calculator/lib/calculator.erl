-module(calculator).

%% API
-export([add/1, multiply/1]).

add(Nums) -> lists:foldl(fun (N, Acc) -> N + Acc end,
			 0,
			 list_to_integers(Nums)).

multiply(Nums) -> lists:foldl(fun (N, Acc) -> N * Acc end,
			      1, 
			      list_to_integers(Nums)).

list_to_integers(List) ->
    lists:map(
      fun(Str) ->
	      case string:to_integer(Str) of
		  {error,_Reason}= E -> E;
		  {Int,_Rest} -> Int
	      end
      end,
      List).
