-module(validatorForward).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init() ->
    validator().

validator() ->
    receive
        
        {validate, Ref, Writes, Client} ->
            send_write_checks(Writes),
            case check_writes(length(Writes)) of
                ok ->
                    update(Writes),
                    Client ! {Ref, ok};
                conflict ->
                    Client ! {Ref, abort}
            end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.

send_write_checks(Writes) ->
    Self = self(),
    lists:foreach(fun({_, Entry, _}) -> Entry ! {check, Self} end, Writes).

check_writes(0) ->
    ok;
check_writes(N) ->
    receive
        no_transaction_conflict ->
            check_writes(N - 1);
        transaction_conflict ->
            conflict
    end.

update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) -> Entry ! {write, Value} end, Writes).
