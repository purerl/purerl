-module(issue36@ps).
-export(['Tuple'/0, patternGuardArray/1, patternGuardArray/0, nonArrayMatch/1, nonArrayMatch/0, doubleArrayPattern/1, doubleArrayPattern/0, doubleArrayMatchGuarded/2, doubleArrayMatchGuarded/0, arrayPatternNoGuard/1, arrayPatternNoGuard/0, arrayMatch/1, arrayMatch/0]).
-compile(nowarn_shadow_vars).
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(no_auto_import).
-include("./issue36.hrl").

-ifndef(PURERL_MEMOIZE).
-define(MEMOIZE(X), X).
-else.
-define(MEMOIZE, memoize).
memoize(X) -> X.
-endif.
-spec 'Tuple'() -> any().
'Tuple'() -> fun (Value0) ->
  fun (Value1) ->
    { tuple, Value0, Value1 }
  end
end.
-spec patternGuardArray(array:array(array:array(array:array(integer())))) -> boolean().
patternGuardArray(_@17) -> 
  V1@99 = fun V1(V2) ->
    false
  end,
  _@20@100 = (array:to_list(_@17)),
  (fun
    Case(_@19, [_@18]) -> 
    _@23 = (array:to_list(_@18)),
    (fun
      Case(_@22, [_@21]) -> 
      _@26 = (array:to_list(_@21)),
      (fun
        Case(_@25, [_@24]) -> 
        case _@24 > 0 of
          (true) -> true;
          (_) -> (V1@99(true))
        end;
        Case(_, _) -> 
        (V1@99(true))
      end(_@21, _@26));
      Case(_, _) -> 
      (V1@99(true))
    end(_@18, _@23));
    Case(_, _) -> 
    (V1@99(true))
  end(_@17, _@20@100))
.
-spec patternGuardArray() -> fun((array:array(array:array(array:array(integer())))) -> boolean()).
patternGuardArray() -> fun (_@27) ->
  (patternGuardArray(_@27))
end.
-spec nonArrayMatch(issue36_Tuple(integer(),any())) -> boolean().
nonArrayMatch(_@30) -> 
  _@32@101 = (fun
    Guard({ tuple, _@31, _ }) -> 
    _@31 =:= 3;
    Guard(_) -> 
    false
  end(_@30)),
  case _@30 of
    ({ tuple, _@31, _ }) when _@32@101 -> true;
    (_) -> false
  end
.
-spec nonArrayMatch() -> fun((issue36_Tuple(integer(),any())) -> boolean()).
nonArrayMatch() -> fun (_@33) ->
  (nonArrayMatch(_@33))
end.
-spec doubleArrayPattern(issue36_Tuple(array:array(integer()),array:array(integer()))) -> boolean().
doubleArrayPattern(_@40) -> 
  _@43@102 = (fun
    ({ tuple, _@42, _ }) -> 
    (array:to_list(_@42));
    (_) -> 
    fail
  end(_@40)),
  _@46@103 = (fun
    ({ tuple, _, _@45 }) -> 
    (array:to_list(_@45));
    (_) -> 
    fail
  end(_@40)),
  (fun
    Case({ tuple, _@42, _ }, [_@41, _], _) -> 
    _@41 =:= 1;
    Case({ tuple, _, _@45 }, _, [_, _@44]) -> 
    _@44 =:= 32;
    Case(_, _, _) -> 
    false
  end(_@40, _@43@102, _@46@103))
.
-spec doubleArrayPattern() -> fun((issue36_Tuple(array:array(integer()),array:array(integer()))) -> boolean()).
doubleArrayPattern() -> fun (_@47) ->
  (doubleArrayPattern(_@47))
end.
-spec doubleArrayMatchGuarded(array:array(integer()),array:array(integer())) -> boolean().
doubleArrayMatchGuarded(_@59,_@60) -> 
  _@64@104 = (array:to_list(_@59)),
  _@66@105 = (fun
    (_@63, _@65, [_@61, _]) -> 
    (array:to_list(_@65));
    (_, _, _) -> 
    fail
  end(_@59, _@60, _@64@104)),
  _@67@106 = (fun
    Guard(_@63, _@65, [_@61, _], [_@62]) -> 
    _@61 > _@62
  end(_@59, _@60, _@64@104, _@66@105)),
  _@70@107 = (array:to_list(_@59)),
  _@71@108 = (fun
    Guard(_@69, _, [_@68, _]) -> 
    _@68 =:= 0
  end(_@59, _@60, _@70@107)),
  (fun
    Case(_@63, _@65, [_@61, _], [_@62], _) when _@67@106 -> 
    true;
    Case(_@69, _, _, _, [_@68, _]) when _@71@108 -> 
    true;
    Case(_, _, _, _, _) -> 
    false
  end(_@59, _@60, _@64@104, _@66@105, _@70@107))
.
-spec doubleArrayMatchGuarded() -> fun((array:array(integer())) -> fun((array:array(integer())) -> boolean())).
doubleArrayMatchGuarded() -> fun (_@72) ->
  fun (_@73) ->
    (doubleArrayMatchGuarded(_@72, _@73))
  end
end.
-spec arrayPatternNoGuard(array:array(integer())) -> boolean().
arrayPatternNoGuard(_@77) -> 
  _@80@109 = (array:to_list(_@77)),
  (fun
    Case(_@79, [_@78, _]) -> 
    _@78 =:= 3;
    Case(_, _) -> 
    false
  end(_@77, _@80@109))
.
-spec arrayPatternNoGuard() -> fun((array:array(integer())) -> boolean()).
arrayPatternNoGuard() -> fun (_@81) ->
  (arrayPatternNoGuard(_@81))
end.
-spec arrayMatch(array:array(integer())) -> boolean().
arrayMatch(_@86) -> 
  _@89@110 = (array:to_list(_@86)),
  _@90@111 = (fun
    Guard(_@88, [_@87, _]) -> 
    _@87 =:= 3
  end(_@86, _@89@110)),
  (fun
    Case(_@88, [_@87, _]) when _@90@111 -> 
    true;
    Case(_, _) -> 
    false
  end(_@86, _@89@110))
.
-spec arrayMatch() -> fun((array:array(integer())) -> boolean()).
arrayMatch() -> fun (_@91) ->
  (arrayMatch(_@91))
end.
