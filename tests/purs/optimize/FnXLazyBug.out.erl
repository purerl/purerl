-module(fnXLazyBug@ps).
-export([zipWith4/5, zipWith4/0]).
-compile(nowarn_shadow_vars).
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(no_auto_import).
-include("./fnXLazyBug.hrl").

-ifndef(PURERL_MEMOIZE).
-define(MEMOIZE(X), X).
-else.
-define(MEMOIZE, memoize).
memoize(X) -> X.
-endif.
-spec '@runtime_lazy'(any()) -> any().
'@runtime_lazy'(CtxRef) -> fun
  (Name, ModuleName, Init) -> 
  StateKey = { Name, ModuleName, lazy_state_@purerl, CtxRef },
  ValueKey = { Name, ModuleName, lazy_value_@purerl, CtxRef },
  fun (LineNo) ->
    case (erlang:get(StateKey)) of
      (initialized) -> (erlang:get(ValueKey));
      (initializing) -> (erlang:throw({ not_finished_initializing, Name, ModuleName, LineNo }));
      (undefined) -> begin
        (erlang:put(StateKey, initializing)),
        Value = (Init(unit)),
        (erlang:put(ValueKey, Value)),
        (erlang:put(StateKey, initialized)),
        Value
      end
    end
  end
end.
-spec zipWith4(fun((any()) -> fun((any()) -> fun((any()) -> fun((any()) -> any())))),any(),any(),any(),any()) -> any().
zipWith4(_@5,_@6,_@7,_@8,_@9) -> 
  LazyCtxRef@10@20@29 = (erlang:make_ref()),
  _@lazy@go@f@21@30 = fun
    Reccase({ _@lazy@go@f, Go@f }) -> 
    (('@runtime_lazy'(LazyCtxRef@10@20@29))(<<"go"/utf8>>, <<"FnXLazyBug"/utf8>>, fun (_) ->
      fun
        (Acc, As1, Bs1, Cs1, Ds1) -> 
        V@25 = (erl_data_list_types@ps:uncons(Ds1)),
        V1@26 = (erl_data_list_types@ps:uncons(Cs1)),
        V2@27 = (erl_data_list_types@ps:uncons(Bs1)),
        V3@28 = (erl_data_list_types@ps:uncons(As1)),
        case { V3@28, V2@27, V1@26, V@25 } of
          ({ { just, _@11 }, { just, _@12 }, { just, _@13 }, { just, _@14 } }) -> (((_@lazy@go@f({ _@lazy@go@f, Go@f }))(13))([((((_@5((maps:get(head, _@11))))((maps:get(head, _@12))))((maps:get(head, _@13))))((maps:get(head, _@14)))) | Acc], (maps:get(tail, _@11)), (maps:get(tail, _@12)), (maps:get(tail, _@13)), (maps:get(tail, _@14))));
          ({ _, _, _, _ }) -> (erl_data_list@ps:reverse(Acc))
        end
      end
    end))
  end,
  Go@f@22@31 = fun
    Reccase({ _@lazy@go@f, Go@f }) -> 
    ((_@lazy@go@f({ _@lazy@go@f, Go@f }))(11))
  end,
  _@lazy@go@23@32 = (_@lazy@go@f@21@30({ _@lazy@go@f@21@30, Go@f@22@31 })),
  Go@24@33 = (Go@f@22@31({ _@lazy@go@f@21@30, Go@f@22@31 })),
  (Go@24@33([], _@6, _@7, _@8, _@9))
.
-spec zipWith4() -> fun((fun((any()) -> fun((any()) -> fun((any()) -> fun((any()) -> any()))))) -> fun((any()) -> fun((any()) -> fun((any()) -> fun((any()) -> any()))))).
zipWith4() -> fun (_@15) ->
  fun (_@16) ->
    fun (_@17) ->
      fun (_@18) ->
        fun (_@19) ->
          (zipWith4(_@15, _@16, _@17, _@18, _@19))
        end
      end
    end
  end
end.
