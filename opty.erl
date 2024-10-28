-module(opty).
-export([start/5, start/6, stop/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(_, _, _, _, _, 0, TotalSum, TotalClients, TotalDiff, MaxDiff, RepetitionCount, Total) ->
    FinalAvg = TotalSum / TotalClients,
    FinalDiff = TotalDiff / RepetitionCount,
    io:format("Final Average: ~w, Average Diff: ~w, Max Diff: ~w, Average Transactions Per Client: ~w, Average Successful transactions ~w ~n", [FinalAvg, FinalDiff, MaxDiff, Total/RepetitionCount, FinalAvg*(Total/RepetitionCount)/100]),
    ok;

start(Clients, Entries, Reads, Writes, Time, Repetitions, TotalSum, TotalClients, TotalDiff, MaxDiff, RepetitionCount, Total) ->
    {RepetitionSum, RepetitionDiff, RepetitionTotal} = start(Clients, Entries, Reads, Writes, Time),

    NewTotalSum = TotalSum + RepetitionSum,
    NewTotalClients = TotalClients + Clients,
    NewMaxDiff = max(MaxDiff, RepetitionDiff),
    NewTotalDiff = TotalDiff + RepetitionDiff,

    start(Clients, Entries, Reads, Writes, Time, Repetitions-1, NewTotalSum, NewTotalClients, NewTotalDiff, NewMaxDiff, RepetitionCount, RepetitionTotal + Total).

start(Clients, Entries, Reads, Writes, Time, Repetitions) ->
    start(Clients, Entries, Reads, Writes, Time, Repetitions, 0, 0, 0, 0, Repetitions, 0).

start(Clients, L, 0) ->
    stop(L, Clients). 


start(Clients, Entries, Reads, Writes, Time) ->
    register(s, server:start(Entries)),

%%    L = startClients(Clients, [], Entries, Reads, Writes),
    Access = 1,
    Part = trunc(Entries * Access),
    L = startClients(Clients, [], Entries, Reads, Writes, Part),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w PART ~w s~n", 
         [Clients, Entries, Reads, Writes, Time, Part]),
    timer:sleep(Time*1000),
    start(Clients, L, 0).

stop(L, Clients) ->
    io:format("Stopping...~n"),
    stopClients(L),
    {RepetitionSum, RepetitionDiff, AverageTotal} = waitClients(L, 0, Clients, 0, 100, 0),
    s ! stop,
    io:format("Stopped~n"),
    {RepetitionSum, RepetitionDiff, AverageTotal}.


startClients(0, L, _, _, _, _) -> L;
startClients(Clients, L, Entries, Reads, Writes, Part) ->
    ShuffledEntries = [X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- lists:seq(1, Entries)])],
    ClientEntries = lists:sublist(ShuffledEntries, Part),
    Pid = client:start(Clients, ClientEntries, Reads, Writes, s),
    startClients(Clients-1, [Pid|L], Entries, Reads, Writes, Part).

stopClients([]) ->
    ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},	
    stopClients(L).

waitClients([], Sum, Clients, MaxRate, MinRate, AllTotal) ->
    Result = Sum/Clients,
    Diff = MaxRate-MinRate,
    io:format("Average ~w, Diff ~w, TotalAvg ~w s~n", [Result, Diff, AllTotal/Clients]),

    {Sum, Diff, AllTotal/Clients};
waitClients(L, Sum, Clients, MaxRate, MinRate, AllTotal) ->
    receive
        {done, Pid, Ok, Total} ->
	    Rate = (Ok/Total),
            waitClients(lists:delete(Pid, L), Sum+Rate, Clients, max(Rate, MaxRate), min(Rate, MinRate), AllTotal + Total)
    end.
