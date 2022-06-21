-module(fnX@ps).
-export([uncurriedFn/0, uncurriedFn/3, uncurried/0, uncurried/3, main/0, curried/3, curried/0]).
-compile(nowarn_shadow_vars).
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(no_auto_import).
-include("./fnX.hrl").

-ifndef(PURERL_MEMOIZE).
-define(MEMOIZE(X), X).
-else.
-define(MEMOIZE, memoize).
memoize(X) -> X.
-endif.
-spec uncurriedFn() -> fun((integer(),integer(),integer()) -> fun((integer()) -> integer())).
uncurriedFn() -> fun
  (A, B, C) -> 
  fun (D) ->
    A + B + C + D
  end
end.
-spec uncurriedFn(integer(),integer(),integer()) -> fun((integer()) -> integer()).
uncurriedFn(_@4,_@5,_@6) -> fun (D) ->
  _@4 + _@5 + _@6 + D
end.
-spec uncurried() -> fun((integer(),integer(),integer()) -> integer()).
uncurried() -> fun
  (A, B, C) -> 
  A + B + C
end.
-spec uncurried(integer(),integer(),integer()) -> integer().
uncurried(_@7,_@8,_@9) -> _@7 + _@8 + _@9.
-spec main() -> integer().
main() -> ((uncurriedFn(1, 2, 3))(4)).
-spec curried(integer(),integer(),integer()) -> integer().
curried(_@10,_@11,_@12) -> _@10 + _@11 + _@12.
-spec curried() -> fun((integer()) -> fun((integer()) -> fun((integer()) -> integer()))).
curried() -> fun (_@13) ->
  fun (_@14) ->
    fun (_@15) ->
      (curried(_@13, _@14, _@15))
    end
  end
end.
