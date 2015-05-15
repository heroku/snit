snit
=====

A SNI terminator library prototype.

Until the project is mature enough, please refer to the [project documentation
for SNI
discovery](https://docs.google.com/document/d/1fYeoDIqyAK3O8ZQwUCP24UqDeCk8AWiRQlI6pGsfX20/edit#)

Build
-----

You'll need an Erlang version of 18-rc2 or newer. You can install one with
[`kerl`](https://github.com/yrashk/kerl) by calling:

    $ kerl build git https://github.com/erlang/otp.git OTP-18.0-rc2 18.0-rc2

Note that on OSX, you will need the newest cipher suites available.

    $ brew update
    $ brew upgrade openssl

Note the version you have (for example, `1.0.2a-1`) and tell `kerl` to use it:

    $ KERL_CONFIGURE_OPTIONS="--with-ssl=/usr/local/Cellar/openssl/1.0.2a-1" build git https://github.com/erlang/otp.git OTP-18.0-rc2 18.0-rc2

The project itself relies on [rebar3](http://www.rebar3.org). [Install a
copy](http://www.rebar3.org/v3.0/docs/getting-started) and compile the project:

    $ rebar3 compile

Tests
-----

    $ rebar3 ct

Browser Tests
-------------

    $ rebar3 shell
    1> application:ensure_all_started(snit).
    snit:start(http, 10, 8080, fun(_) -> [
        {certfile, "test/snit_basic_SUITE_data/cert.pem"},
        {keyfile, "test/snit_basic_SUITE_data/key.pem"},
        {cacertfile, "test/snit_basic_SUITE_data/cacerts.pem"}
    ] end, snit_http_hello, []).

Then open a browser to `$HOSTNAME:8080`. Do note that exceptions will need
to be added on browsers (the test certificates use the OTP CA), and Chrome
may never be happy with a custom certificate on localhost.

Changelog
---------

Nothing's stable. Nor working.
