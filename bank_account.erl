-module(bank_account).

-export([balance/1, charge/2, close/1, create/0, deposit/2, withdraw/2, ba_process/1]).

transaction(Pid, Type) ->
    Pid ! {self(), Type},
    receive
        Amount -> Amount
    end.

transaction(Pid, Type, Amount) ->
    Pid ! {self(), Type, Amount},
    receive
        Message -> Message
    end.

balance(Pid) -> transaction(Pid, balance).

charge(Pid, Amount) -> transaction(Pid, charge, Amount).

close(Pid) -> transaction(Pid, close).


create() ->
    spawn(bank_account, ba_process, [0]).


ba_close() ->
    receive
        {Pid, _ } -> Pid ! {error, account_closed}, ba_close();
        {Pid, _, _} -> Pid ! {error, account_closed}, ba_close()
    end.

ba_process(Balance) ->
    receive
        {Pid, deposit, Amount} when Amount >= 0 -> 
            Pid ! Amount,
            ba_process(Balance + Amount);

        {Pid, deposit, Amount} when Amount < 0 -> 
            Pid ! 0,
            ba_process(Balance);

        {Pid, withdraw, Amount} when Amount < 0 -> 
            Pid ! 0,
            ba_process(Balance); 

        {Pid, withdraw, Amount} when Amount =< Balance ->
            Pid ! Amount,
            ba_process(Balance - Amount);

        {Pid, withdraw, Amount} when Amount > Balance ->
            Pid ! Balance,
            ba_process(0);
            
        {Pid, charge, Amount} when Amount < 0 -> 
            Pid ! 0,
            ba_process(Balance); 
        {Pid, charge, Amount} when Amount =< Balance ->
            Pid ! Amount,
            ba_process(Balance - Amount);
        {Pid, charge, Amount} when Amount > Balance ->
            Pid ! 0,
            ba_process(Balance);

        {Pid, balance} -> Pid ! Balance, ba_process(Balance);
        {Pid, close} -> Pid ! Balance, ba_close()
    end.

deposit(Pid, Amount) -> transaction(Pid, deposit, Amount).


withdraw(Pid, Amount) -> transaction(Pid, withdraw, Amount).

