-module(snit_util).

-export([validate/1]).

validate(Certs) ->
    try
        case length(Certs) of
            2 ->
                true = lists:keymember(key, 1, Certs),
                true = lists:keymember(cert, 1, Certs),
                self_signed;
            3->
                true = lists:keymember(key, 1, Certs),
                true = lists:keymember(cacerts, 1, Certs),
                true = lists:keymember(cert, 1, Certs),
                valid
        end
    catch _:_ ->
            invalid
    end.
