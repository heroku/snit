-module(mock_pallet).

-compile([export_all]).


-define(PLAINTEXT, <<"AK9WhXyHnpSydGsm08dZlpfLujTG/yZLYJ8RUJhL7xk=">>).
-define(CIPHERTEXT, <<"CiAxBf2XHX/7NXWCFoQxngNEYSXK6hotwM7TQDCA2RGZ5xKnAQEBAwB"
                      "4MQX9lx1/+zV1ghaEMZ4DRGElyuoaLcDO00AwgNkRmecAAAB+MHwGCS"
                      "qGSIb3DQEHBqBvMG0CAQAwaAYJKoZIhvcNAQcBMB4GCWCGSAFlAwQBLj"
                      "ARBAw+45eCnc+KXioa4hYCARCAO5FWAF89HAxOzEVcdUF6iyj2gh0N1s"
                      "vWFZFRhlm9asigk3qPusrsQkC3HorK9McnvROO9oJvP4jwgEts">>).

-include_lib("pallet/src/pallet.hrl").

setup() ->
    os:putenv("AWS_ACCESS_KEY", "xxx"),
    os:putenv("AWS_SECRET_KEY", "xxx"),
    meck:new(pallet_kms, [passthrough, no_link]),
    meck:expect(pallet_kms, get_creds,
        fun() ->
            {ok, #kms_creds{
              aws_creds = #{},
              kms_key_alias = <<>>}}
        end),
    meck:expect(pallet_kms, encrypt,
              fun(_Client, Plaintext) ->
                  Key = fernet:generate_token(Plaintext, ?PLAINTEXT),
                  {ok, Key, ?CIPHERTEXT}
              end),
    ok.

teardown() ->
    meck:unload(pallet_kms),
    ok.