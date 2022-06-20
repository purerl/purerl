-module(passing4179@foreign).

-export([assertThrows/1]).

assertThrows(F) -> fun () ->
  try
    F(unit),
    throw(no_catch)
  catch
    _:{not_finished_initializing, Name, Module, Line}
      -> {Name, Module, Line}
  end
end.