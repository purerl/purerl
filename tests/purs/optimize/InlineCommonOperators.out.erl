-module(inlineCommonOperators@ps).
-export(['N'/0, newtypeN_/0, inlineWrap/0, inlineVoid/0, inlineUnwrap/0, inlineUnsafeCoerce/0, inlineUnsafeCoerce/1, inlineUnary/0, inlineOver2/0, inlineOver2/2, inlineOver/0, inlineOver/1, inlineListSingleton/0, inlineListCons/2, inlineListCons/0, inlineIntToNumber/0, inlineCoerce/0, inlineBinary/0, inlineAtom/0]).
-compile(nowarn_shadow_vars).
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(no_auto_import).
-include("./inlineCommonOperators.hrl").

-ifndef(PURERL_MEMOIZE).
-define(MEMOIZE(X), X).
-else.
-define(MEMOIZE, memoize).
memoize(X) -> X.
-endif.
-spec add() -> any().
add() -> (?MEMOIZE((data_semiring@ps:add((data_semiring@ps:semiringInt()))))).
-spec 'N'() -> any().
'N'() -> fun (X) ->
  X
end.
-spec newtypeN_() -> any().
newtypeN_() -> #{'Coercible0'=>fun (_) ->
  undefined
end}.
-spec inlineWrap() -> inlineCommonOperators_N(integer()).
inlineWrap() -> 1.
-spec inlineVoid() -> fun(() -> any()).
inlineVoid() -> (data_functor@ps:void((effect@ps:functorEffect()), (control_applicative@ps:pure((effect@ps:applicativeEffect()), 49)))).
-spec inlineUnwrap() -> integer().
inlineUnwrap() -> 1.
-spec inlineUnsafeCoerce() -> fun((integer()) -> binary()).
inlineUnsafeCoerce() -> 42.
-spec inlineUnsafeCoerce(integer()) -> binary().
inlineUnsafeCoerce(_@13) -> ((inlineUnsafeCoerce())(_@13)).
-spec inlineUnary() -> any().
inlineUnary() -> 
  NotBoolean@30 = not false,
  NegNum@31 = - 1.2,
  NegInt@32 = - 1,
  unit
.
-spec inlineOver2() -> fun((inlineCommonOperators_N(integer())) -> fun((inlineCommonOperators_N(integer())) -> inlineCommonOperators_N(integer()))).
inlineOver2() -> (add()).
-spec inlineOver2(inlineCommonOperators_N(integer()),inlineCommonOperators_N(integer())) -> inlineCommonOperators_N(integer()).
inlineOver2(_@16,_@17) -> (((inlineOver2())(_@16))(_@17)).
-spec inlineOver() -> fun((inlineCommonOperators_N(integer())) -> inlineCommonOperators_N(integer())).
inlineOver() -> fun (V) ->
  V + 1
end.
-spec inlineOver(inlineCommonOperators_N(integer())) -> inlineCommonOperators_N(integer()).
inlineOver(_@19) -> ((inlineOver())(_@19)).
-spec inlineListSingleton() -> any().
inlineListSingleton() -> [1].
-spec inlineListCons(integer(),any()) -> any().
inlineListCons(_@22,_@23) -> case { _@22, _@23 } of
  ({ _@24, _@25 }) -> [_@24 | _@25]
end.
-spec inlineListCons() -> fun((integer()) -> fun((any()) -> any())).
inlineListCons() -> fun (_@26) ->
  fun (_@27) ->
    (inlineListCons(_@26, _@27))
  end
end.
-spec inlineIntToNumber() -> float().
inlineIntToNumber() -> (erlang:float(42)).
-spec inlineCoerce() -> integer().
inlineCoerce() -> 42.
-spec inlineBinary() -> any().
inlineBinary() -> 
  SubNum@33 = 1.0 - 2.0,
  SubInt@34 = 1 - 2,
  OrBool@35 = true orelse false,
  NotInt@36 = 1 =/= 2,
  NotEqString@37 = <<"aaa"/utf8>> =/= <<"bbb"/utf8>>,
  NotEqNum@38 = 1.0 =/= 2.0,
  NotEqChar@39 = $a =/= $b,
  NotEqBoolean@40 = false =:= false,
  MulNum@41 = 1.0 * 2.0,
  MulInt@42 = 1 * 2,
  MinInt@43 = (erlang:min(1, 2)),
  MaxInt@44 = (erlang:max(1, 2)),
  LteInt@45 = 1 =< 2,
  LtInt@46 = 1 < 2,
  GteInt@47 = 1 >= 2,
  GtInt@48 = 1 > 2,
  EqString@49 = <<"aaa"/utf8>> =:= <<"bbb"/utf8>>,
  EqNum@50 = 1.0 =:= 2.0,
  EqInt@51 = 1 =:= 2,
  EqChar@52 = $a =:= $b,
  EqBoolean@53 = false =:= false,
  DivNum@54 = 1.0 / 2.0,
  DivInt@55 = 1 div 2,
  AppendList@56 = fun AppendList(V) ->
    fun (L2) ->
      case { V, L2 } of
        ({ _@28, _@29 }) -> _@28 ++ _@29
      end
    end
  end,
  AndBool@57 = true andalso false,
  AddNum@58 = 1.0 + 2.0,
  AddInt@59 = 1 + 2,
  unit
.
-spec inlineAtom() -> any().
inlineAtom() -> an_atom.
