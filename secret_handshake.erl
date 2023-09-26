-module(secret_handshake).

-export([commands/1]).

bin_to_command(1, Acc) -> ["wink" | Acc]; 
bin_to_command(2, Acc) -> ["double blink" | Acc]; 
bin_to_command(4, Acc) -> ["close your eyes" | Acc]; 
bin_to_command(8, Acc) -> ["jump" | Acc]; 
bin_to_command(16, Acc) -> lists:reverse(Acc).

commands(Number) -> lists:reverse(commands(Number, 1, [])).

commands(_Number, 32, Acc) -> Acc;
commands(Number, TwoAdN, Acc) when Number band TwoAdN =:= TwoAdN -> commands(Number, TwoAdN * 2, bin_to_command(TwoAdN, Acc));
commands(Number, TwoAdN, Acc) -> commands(Number, TwoAdN * 2, Acc).
