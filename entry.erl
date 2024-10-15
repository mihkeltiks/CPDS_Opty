-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref()).

entry(Value, Time) ->
    receive
        {read, Ref, From} ->
            Pid = self(),
            From ! {Ref, Value, Time, Pid},
            entry(Value, Time);
        {write, New} ->
            entry(New , make_ref());  
        {check, Ref, Readtime, From} ->
            if 
                 Time == Readtime ->   From ! {Ref, ok};
                true ->
                    From ! {Ref, abort}
            end,
            entry(Value, Time);
        stop ->
            ok
    end.
