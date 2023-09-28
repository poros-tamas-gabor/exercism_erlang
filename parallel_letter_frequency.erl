-module(parallel_letter_frequency).

-export([dict/1, count_letter/3]).


count_letter(_String = [H | T], AccDict, Pid) ->
    count_letter(T, dict:update_counter(H, 1, AccDict), Pid);
count_letter([], AccDict, Pid) -> Pid ! AccDict.

listen(AccDict, Counter, Max) when Counter < Max ->
    receive
        Dict -> 
            NewDict = dict:merge(fun(_K, V1, V2) -> V1+V2 end, Dict, AccDict),
            listen(NewDict, Counter + 1, Max)
    end;
listen(AccDict, Max, Max) -> AccDict.

dict(Strings) ->
    [spawn(parallel_letter_frequency, count_letter, [Word, dict:new(), self()]) || Word <- Strings],
    listen(dict:new(), 0, length(Strings)).


