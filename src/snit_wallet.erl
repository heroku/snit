-module(snit_wallet).

-export([
         make/2, make_fake/0,
         roll_key/3, roll_key/4,
         epoch/1,
         key/2
        ]).

-define(shard_count, 128).
-define(v1, 1).

-record(wallet,
        {
          %% version will likely go largely unused until we have more
          %% than one.
          version = ?v1 :: non_neg_integer(),
          epoch :: non_neg_integer(),
          keys :: tuple()
        }).

-opaque wallet() :: #wallet{}.
-type key() :: binary().

-export_type([wallet/0, key/0]).


make(Epoch, KeyList) ->
    %% validate keylist here
    #wallet{epoch = Epoch,
            keys = list_to_tuple(KeyList)}.

make_fake() ->
    Keys = list_to_tuple([fernet:generate_encoded_key()
                          || _ <- lists:seq(1, ?shard_count)]),
    #wallet{epoch = 0,
            keys = Keys}.

%% not sure that we actually need this? maybe a function like stale()
%% to compare wallet versions?
epoch(#wallet{epoch = Epoch}) ->
    Epoch.

-spec key(binary(), wallet()) -> key().
key(Domain, #wallet{keys = Keys}) ->
    Shard = get_key(Domain),
    element(Shard, Keys).

roll_key(Shard, NewKey, Wallet) ->
    roll_key(Shard, NewKey, Wallet, true).

roll_key(Shard, NewKey, Wallet, AdvanceEpoch) ->
    OldKeys = Wallet#wallet.keys,
    NewKeys = setelement(Shard, OldKeys, NewKey),
    OldEpoch = Wallet#wallet.epoch,
    NewEpoch =
        case AdvanceEpoch of
            true -> OldEpoch + 1;
            false -> OldEpoch
        end,
    Wallet#wallet{keys = NewKeys, epoch = NewEpoch}.

get_key(Domain) ->
    (erlang:phash2(Domain) rem ?shard_count) + 1.
