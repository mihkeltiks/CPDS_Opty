-module(opty).
-export([start/5, start/6, stop/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(_, _, _, _, _, 0, TotalSum, TotalClients, TotalDiff, MaxDiff, RepetitionCount) ->
    FinalAvg = TotalSum / TotalClients,
    FinalDiff = TotalDiff / RepetitionCount,
    io:format("Final Average: ~w, Average Diff: ~w, Max Diff: ~w ~n", [FinalAvg, FinalDiff, MaxDiff]),
    ok;

start(Clients, Entries, Reads, Writes, Time, Repetitions, TotalSum, TotalClients, TotalDiff, MaxDiff, RepetitionCount) ->
    {RepetitionSum, RepetitionDiff} = start(Clients, Entries, Reads, Writes, Time),

    NewTotalSum = TotalSum + RepetitionSum,
    NewTotalClients = TotalClients + Clients,
    NewMaxDiff = max(MaxDiff, RepetitionDiff),
    NewTotalDiff = TotalDiff + RepetitionDiff,

    start(Clients, Entries, Reads, Writes, Time, Repetitions-1, NewTotalSum, NewTotalClients, NewTotalDiff, NewMaxDiff, RepetitionCount).

start(Clients, Entries, Reads, Writes, Time, Repetitions) ->
    start(Clients, Entries, Reads, Writes, Time, Repetitions, 0, 0, 0, 0, Repetitions).

start(Clients, L, 0) ->
    stop(L, Clients). 


start(Clients, Entries, Reads, Writes, Time) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Reads, Writes),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", 
         [Clients, Entries, Reads, Writes, Time]),
    timer:sleep(Time*1000),
    start(Clients, L, 0).

stop(L, Clients) ->
    io:format("Stopping...~n"),
    stopClients(L),
    {RepetitionSum, RepetitionDiff} = waitClients(L, 0, Clients, 0, 100),
    s ! stop,
    io:format("Stopped~n"),
    {RepetitionSum, RepetitionDiff}.


startClients(0, L, _, _, _) -> L;
startClients(Clients, L, Entries, Reads, Writes) ->
    Pid = client:start(Clients, Entries, Reads, Writes, s),
    startClients(Clients-1, [Pid|L], Entries, Reads, Writes).

stopClients([]) ->
    ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},	
    stopClients(L).

waitClients([], Sum, Clients, MaxRate, MinRate) ->
    Result = Sum/Clients,
    Diff = MaxRate-MinRate,
    io:format("Average ~w, Diff ~w s~n", [Result, Diff]),
    {Sum, Diff};
waitClients(L, Sum, Clients, MaxRate, MinRate) ->
    receive
        {done, Pid, Rate} ->
            waitClients(lists:delete(Pid, L), Sum+Rate, Clients, max(Rate, MaxRate), min(Rate, MinRate))
    end.
