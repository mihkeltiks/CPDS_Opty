-module(entryForward).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    ActiveTransactions = [],
    entry(Value, make_ref(), ActiveTransactions).

entry(Value, Time, ActiveTransactions) ->
    receive
        {read, Ref, From} ->
            From ! {Ref, self(), Value, Time},
            UpdatedTransactions = [{From, Ref} | ActiveTransactions],
            entry(Value, Time, UpdatedTransactions);
            
        {check, ValidatorPid} ->
            case ActiveTransactions of
                [] ->  
                    ValidatorPid ! no_transaction_conflict;
                [_ | _] ->  
                    ValidatorPid ! transaction_conflict
            end,
            entry(Value, Time, ActiveTransactions);

            
        {write, NewValue} ->
            entry(NewValue, make_ref(), ActiveTransactions);

        {done, From} ->
            UpdatedTransactions = [{Pid, Ref} || {Pid, Ref} <- ActiveTransactions, Pid =/= From],
            entry(Value, Time, UpdatedTransactions);
            
        stop ->
            ok
    end.
