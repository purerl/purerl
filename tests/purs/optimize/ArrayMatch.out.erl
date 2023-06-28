-module(arrayMatch@ps).
-export([onlyArray/1, onlyArray/0, nestedArrayViaRecord/1, nestedArrayViaRecord/0, nestedArrayRefutable2/2, nestedArrayRefutable2/0, nestedArrayRefutable/2, nestedArrayRefutable/0, nestedArray/1, nestedArray/0, namedArray/1, namedArray/0, maybeArray/1, maybeArray/0, bug28_2/1, bug28_2/0, bug28/1, bug28/0, main/0]).
-compile(nowarn_shadow_vars).
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(no_auto_import).
-include("./arrayMatch.hrl").

-ifndef(PURERL_MEMOIZE).
-define(MEMOIZE(X), X).
-else.
-define(MEMOIZE, memoize).
memoize(X) -> X.
-endif.
-spec onlyArray(array:array(integer())) -> integer().
onlyArray(_@16) -> 
  _@20@150 = (array:to_list(_@16)),
  (fun
    Case(_@19, [_@17, _@18]) -> 
    _@17 + _@18;
    Case(_, _) -> 
    0
  end(_@16, _@20@150))
.
-spec onlyArray() -> fun((array:array(integer())) -> integer()).
onlyArray() -> fun (_@21) ->
  (onlyArray(_@21))
end.
-spec nestedArrayViaRecord(map()) -> integer().
nestedArrayViaRecord(_@36) -> 
  _@42@151 = (fun
    (#{q:=_@39}) -> 
    (array:to_list(_@39));
    (_) -> 
    fail
  end(_@36)),
  _@41@152 = (fun
    (#{q:=_@39}, [#{r:=_@40}]) -> 
    (array:to_list(_@40));
    (_, _) -> 
    fail
  end(_@36, _@42@151)),
  _@50@153 = (fun
    (#{q:=_@45}) -> 
    (array:to_list(_@45));
    (_) -> 
    fail
  end(_@36)),
  _@47@154 = (fun
    (#{q:=_@45}, [#{r:=_@46}, #{r:=_@48}]) -> 
    (array:to_list(_@46));
    (_, _) -> 
    fail
  end(_@36, _@50@153)),
  _@49@155 = (fun
    (#{q:=_@45}, [#{r:=_@46}, #{r:=_@48}], [_@43]) -> 
    (array:to_list(_@48));
    (_, _, _) -> 
    fail
  end(_@36, _@50@153, _@47@154)),
  (fun
    Case(#{q:=_@39}, [#{r:=_@40}], [_@37, _@38], _, _, _) -> 
    _@37 + _@38;
    Case(#{q:=_@45}, _, _, [#{r:=_@46}, #{r:=_@48}], [_@43], [_@44]) -> 
    _@43 + _@44;
    Case(_, _, _, _, _, _) -> 
    0
  end(_@36, _@42@151, _@41@152, _@50@153, _@47@154, _@49@155))
.
-spec nestedArrayViaRecord() -> fun((map()) -> integer()).
nestedArrayViaRecord() -> fun (_@51) ->
  (nestedArrayViaRecord(_@51))
end.
-spec nestedArrayRefutable2(array:array(array:array(integer())),integer()) -> integer().
nestedArrayRefutable2(_@58,_@59) -> 
  _@65@156 = (fun
    (_@62, 2) -> 
    (array:to_list(_@62));
    (_, _) -> 
    fail
  end(_@58, _@59)),
  _@64@157 = (fun
    (_@62, 2, [_@63, _]) -> 
    (array:to_list(_@63));
    (_, _, _) -> 
    fail
  end(_@58, _@59, _@65@156)),
  (fun
    Case(_@62, 2, [_@63, _], [_@60, _@61, 3]) -> 
    _@60 + _@61 + 1;
    Case(_, _, _, _) -> 
    0
  end(_@58, _@59, _@65@156, _@64@157))
.
-spec nestedArrayRefutable2() -> fun((array:array(array:array(integer()))) -> fun((integer()) -> integer())).
nestedArrayRefutable2() -> fun (_@66) ->
  fun (_@67) ->
    (nestedArrayRefutable2(_@66, _@67))
  end
end.
-spec nestedArrayRefutable(array:array(array:array(integer())),integer()) -> integer().
nestedArrayRefutable(_@74,_@75) -> 
  _@81@158 = (fun
    (_@78, 2) -> 
    (array:to_list(_@78));
    (_, _) -> 
    fail
  end(_@74, _@75)),
  _@80@159 = (fun
    (_@78, 2, [_@79, _]) -> 
    (array:to_list(_@79));
    (_, _, _) -> 
    fail
  end(_@74, _@75, _@81@158)),
  (fun
    Case(_@78, 2, [_@79, _], [_@76, _@77, 3]) -> 
    _@76 + _@77 + 1;
    Case(_, _, _, _) -> 
    0
  end(_@74, _@75, _@81@158, _@80@159))
.
-spec nestedArrayRefutable() -> fun((array:array(array:array(integer()))) -> fun((integer()) -> integer())).
nestedArrayRefutable() -> fun (_@82) ->
  fun (_@83) ->
    (nestedArrayRefutable(_@82, _@83))
  end
end.
-spec nestedArray(array:array(array:array(integer()))) -> integer().
nestedArray(_@90) -> 
  _@96@160 = (array:to_list(_@90)),
  _@95@161 = (fun
    (_@93, [_@94, _]) -> 
    (array:to_list(_@94));
    (_, _) -> 
    fail
  end(_@90, _@96@160)),
  (fun
    Case(_@93, [_@94, _], [_@91, _@92]) -> 
    _@91 + _@92 + 1;
    Case(_, _, _) -> 
    0
  end(_@90, _@96@160, _@95@161))
.
-spec nestedArray() -> fun((array:array(array:array(integer()))) -> integer()).
nestedArray() -> fun (_@97) ->
  (nestedArray(_@97))
end.
-spec namedArray(array:array(integer())) -> integer().
namedArray(_@103) -> 
  _@108@162 = (fun
    (_@104 = _@107) -> 
    (array:to_list(_@107))
  end(_@103)),
  (fun
    Case(_@104 = _@107, [_@105, _@106]) -> 
    _@105 + _@106;
    Case(_, _) -> 
    0
  end(_@103, _@108@162))
.
-spec namedArray() -> fun((array:array(integer())) -> integer()).
namedArray() -> fun (_@109) ->
  (namedArray(_@109))
end.
-spec maybeArray(data_maybe_Maybe(array:array(integer()))) -> integer().
maybeArray(_@114) -> 
  _@118@163 = (fun
    ({ just, _@117 }) -> 
    (array:to_list(_@117));
    (_) -> 
    fail
  end(_@114)),
  (fun
    Case({ just, _@117 }, [_@115, _@116]) -> 
    _@115 + _@116;
    Case(_, _) -> 
    0
  end(_@114, _@118@163))
.
-spec maybeArray() -> fun((data_maybe_Maybe(array:array(integer()))) -> integer()).
maybeArray() -> fun (_@119) ->
  (maybeArray(_@119))
end.
-spec bug28_2(any()) -> integer().
bug28_2(_@125) -> case #{q=>(array:from_list([1, 2]))} of
  (#{q:=_@126}) -> begin
    _@130 = (array:to_list(_@126)),
    (fun
      Case(_@129, [_@127, _@128]) -> 
      _@127 + _@128;
      Case(_, _) -> 
      0
    end(_@126, _@130))
  end;
  (_) -> 0
end.
-spec bug28_2() -> fun((any()) -> integer()).
bug28_2() -> fun (_@131) ->
  (bug28_2(_@131))
end.
-spec bug28(map()) -> integer().
bug28(_@136) -> 
  _@140@164 = (fun
    (#{q:=_@139}) -> 
    (array:to_list(_@139));
    (_) -> 
    fail
  end(_@136)),
  (fun
    Case(#{q:=_@139}, [_@137, _@138]) -> 
    _@137 + _@138;
    Case(_, _) -> 
    0
  end(_@136, _@140@164))
.
-spec bug28() -> fun((map()) -> integer()).
bug28() -> fun (_@141) ->
  (bug28(_@141))
end.
-spec main() -> fun(() -> any()).
main() -> fun
  __do() -> 
  (((?MEMOIZE((effect_class_console@ps:log((effect_class@ps:monadEffectEffect())))))(((?MEMOIZE((data_show@ps:show((data_show@ps:showInt())))))((bug28(#{q=>(array:from_list([1, 2]))}))))))()),
  (((?MEMOIZE((effect_class_console@ps:log((effect_class@ps:monadEffectEffect())))))(((?MEMOIZE((data_show@ps:show((data_show@ps:showInt())))))((bug28_2(#{q=>(array:from_list([1, 2]))}))))))()),
  (((?MEMOIZE((effect_class_console@ps:log((effect_class@ps:monadEffectEffect())))))(((?MEMOIZE((data_show@ps:show((data_show@ps:showInt())))))((nestedArray((array:from_list([(array:from_list([1, 2])), (array:from_list([3]))]))))))))()),
  (((?MEMOIZE((effect_class_console@ps:log((effect_class@ps:monadEffectEffect())))))(((?MEMOIZE((data_show@ps:show((data_show@ps:showInt())))))((nestedArrayViaRecord(#{q=>(array:from_list([#{r=>(array:from_list([1, 2]))}, #{r=>(array:from_list([3]))}]))}))))))()),
  (((?MEMOIZE((effect_class_console@ps:log((effect_class@ps:monadEffectEffect())))))(((?MEMOIZE((data_show@ps:show((data_show@ps:showInt())))))((onlyArray((array:from_list([1]))))))))()),
  (((?MEMOIZE((effect_class_console@ps:log((effect_class@ps:monadEffectEffect())))))(((?MEMOIZE((data_show@ps:show((data_show@ps:showInt())))))((maybeArray({ just, (array:from_list([1, 2])) }))))))()),
  (((?MEMOIZE((effect_class_console@ps:log((effect_class@ps:monadEffectEffect())))))(((?MEMOIZE((data_show@ps:show((data_show@ps:showInt())))))((namedArray((array:from_list([1, 2]))))))))()),
  (((?MEMOIZE((effect_class_console@ps:log((effect_class@ps:monadEffectEffect())))))(<<"Done"/utf8>>))())
end.
