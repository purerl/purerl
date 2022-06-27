-module(dataBounded@ps).
-export([test3/0, test2/0, test1/0]).
-compile(nowarn_shadow_vars).
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(no_auto_import).
-include("./dataBounded.hrl").

-ifndef(PURERL_MEMOIZE).
-define(MEMOIZE(X), X).
-else.
-define(MEMOIZE, memoize).
memoize(X) -> X.
-endif.
-spec test3() -> boolean().
test3() -> true orelse false.
-spec test2() -> boolean().
test2() -> false.
-spec test1() -> boolean().
test1() -> true.
