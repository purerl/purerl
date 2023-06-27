-module(magicDo@ps).
-export(['MyEnv'/0, 'Efect'/0, 'E'/0, monadEfect/0, monadEffectEfect/0, bindEfect/0, applicativeEfect/0, monadE/0, functorE/0, bindE/0, egE/0, applyE/0, applicativeE/0, action/0, 'action\''/0, ee/0, er/0, foo/0, newt/0]).
-compile(nowarn_shadow_vars).
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(no_auto_import).
-include("./magicDo.hrl").

-ifndef(PURERL_MEMOIZE).
-define(MEMOIZE(X), X).
-else.
-define(MEMOIZE, memoize).
memoize(X) -> X.
-endif.
-spec pure() -> any().
pure() -> (?MEMOIZE((control_applicative@ps:pure((magicDo_eE@ps:applicativeEE()))))).
-spec pure(any()) -> any().
pure(_@3) -> ((pure())(_@3)).
-spec discard1(any(),any()) -> any().
discard1(_@6,_@7) -> (((control_bind@ps:bind((magicDo_eE@ps:bindEE())))(_@6))(_@7)).
-spec liftEffect() -> any().
liftEffect() -> (?MEMOIZE((effect_class@ps:liftEffect((magicDo_eE@ps:monadEffectEE()))))).
-spec liftEffect(any()) -> any().
liftEffect(_@9) -> ((liftEffect())(_@9)).
-spec bindReaderT() -> any().
bindReaderT() -> ((control_monad_reader_trans@ps:bindReaderT())((effect@ps:bindEffect()))).
-spec pure1() -> any().
pure1() -> (?MEMOIZE((control_applicative@ps:pure(((control_monad_reader_trans@ps:applicativeReaderT())((effect@ps:applicativeEffect()))))))).
-spec pure1(any()) -> any().
pure1(_@15) -> ((pure1())(_@15)).
-spec discard2(any(),any()) -> any().
discard2(_@18,_@19) -> (((control_bind@ps:bind(((control_monad_reader_trans@ps:bindReaderT())((effect@ps:bindEffect())))))(_@18))(_@19)).
-spec liftEffect1() -> any().
liftEffect1() -> (?MEMOIZE((effect_class@ps:liftEffect(((control_monad_reader_trans@ps:monadEffectReader())((effect_class@ps:monadEffectEffect()))))))).
-spec liftEffect1(any()) -> any().
liftEffect1(_@21) -> ((liftEffect1())(_@21)).
-spec 'MyEnv'() -> any().
'MyEnv'() -> { myEnv }.
-spec 'Efect'() -> any().
'Efect'() -> fun (X) ->
  X
end.
-spec 'E'() -> any().
'E'() -> fun (X) ->
  X
end.
-spec monadEfect() -> any().
monadEfect() -> (effect@ps:monadEffect()).
-spec monadEffectEfect() -> any().
monadEffectEfect() -> #{liftEffect=>('Efect'()), 'Monad0'=>fun (_) ->
  (monadEfect())
end}.
-spec bindEfect() -> any().
bindEfect() -> (effect@ps:bindEffect()).
-spec applicativeEfect() -> any().
applicativeEfect() -> (effect@ps:applicativeEffect()).
-spec monadE() -> any().
monadE() -> (effect@ps:monadEffect()).
-spec functorE() -> any().
functorE() -> (effect@ps:functorEffect()).
-spec bindE() -> any().
bindE() -> (effect@ps:bindEffect()).
-spec egE() -> magicDo_E(any()).
egE() -> fun
  __do() -> 
  ((effect_console@ps:log(<<"abc"/utf8>>))()),
  ((effect_console@ps:log(<<"abc"/utf8>>))()),
  ((effect_console@ps:log(<<"abc"/utf8>>))())
end.
-spec applyE() -> any().
applyE() -> (effect@ps:applyEffect()).
-spec applicativeE() -> any().
applicativeE() -> (effect@ps:applicativeEffect()).
-spec action() -> fun(() -> any()).
action() -> (effect_console@ps:log(<<"blah"/utf8>>)).
-spec 'action\''() -> magicDo_Efect(any()).
'action\''() -> (action()).
-spec ee() -> magicDo_eE_EE(integer()).
ee() -> (control_bind@ps:bind((magicDo_eE@ps:bindEE()), (pure(42)), fun (Foo1) ->
  (discard1((liftEffect((action()))), fun (_) ->
    (pure(Foo1 + 32))
  end))
end)).
-spec er() -> any().
er() -> (control_bind@ps:bind((bindReaderT()), (pure1(42)), fun (Foo1) ->
  (discard2((liftEffect1((action()))), fun (_) ->
    (pure1(Foo1 + 32))
  end))
end)).
-spec foo() -> fun(() -> integer()).
foo() -> fun
  __do() -> 
  ((action())()),
  Foo1@49 = 42,
  ((action())()),
  Bar@48@50 = Foo1@49 + 1,
  14
end.
-spec newt() -> magicDo_Efect(integer()).
newt() -> fun
  __do() -> 
  ((action())()),
  Foo1@52 = 42,
  ((action())()),
  (((?MEMOIZE((effect_class@ps:liftEffect((monadEffectEfect())))))((action())))()),
  Bar@51@53 = Foo1@52 + 1,
  13
end.
