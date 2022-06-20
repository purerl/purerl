-module(foreignTypes@foreign).

-export([test/2]).

% -spec test() -> fun((data_maybe_Maybe(any())) -> fun((fun((any()) -> any())) -> data_maybe_Maybe(integer()))).
% test() -> fun (_@0) ->
%   fun (_@1) ->
%     (foreignTypes@foreign:test(_@0, _@1))
%   end
% end.

test(X,Y) -> {nothing}.
% test(X) -> fun (Y) -> {nothing} end.

% -spec test() -> fun((data_maybe_Maybe(any())) -> fun((fun((any()) -> any())) -> data_maybe_Maybe(integer()))).
% test() -> fun (_@0) ->
%   fun (_@1) ->
%     ((foreignTypes@foreign:test(_@0))(_@1))
%   end
% end.