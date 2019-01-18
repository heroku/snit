%%%-------------------------------------------------------------------
%%% Copyright (c) 2015-2017, Heroku Inc <routing-feedback@heroku.com>,
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%% 
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%% 
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%% 
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(snit_util).

-export([validate/1, supported/0]).

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

-ifdef(OLD_CIPHERSUITES).
supported() ->
    ssl:cipher_suites(erlang).
-else.
supported() ->
    Vsns = ['tlsv1.2', 'tlsv1.1', 'tlsv1'],
    lists:usort(lists:append([ssl:cipher_suites(all, Vsn) || Vsn <- Vsns])).
-endif.

