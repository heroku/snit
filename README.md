[![Build Status](https://magnum.travis-ci.com/heroku/snit.svg?token=uNxDxTYyzRaxPpJGQ5yq&branch=master)](https://magnum.travis-ci.com/heroku/snit)

snit
=====

A SNI terminator library prototype.

Until the project is mature enough, please refer to the [project documentation
for SNI
discovery](https://docs.google.com/document/d/1fYeoDIqyAK3O8ZQwUCP24UqDeCk8AWiRQlI6pGsfX20/edit#)

Build
-----

You'll need an Erlang version of 18.0 or newer. You can install one with
[`kerl`](https://github.com/yrashk/kerl) by calling:

    $ kerl build git https://github.com/erlang/otp.git OTP-18.0 18.0

Note that on OSX, you will need the newest cipher suites available.

    $ brew update
    $ brew upgrade openssl

Note the version you have (for example, `1.0.2a-1`) and tell `kerl` to use it:

    $ KERL_CONFIGURE_OPTIONS="--with-ssl=/usr/local/Cellar/openssl/1.0.2a-1" build git https://github.com/erlang/otp.git OTP-18.0 18.0

The project itself relies on [rebar3](http://www.rebar3.org). [Install a
copy](http://www.rebar3.org/v3.0/docs/getting-started) and compile the project:

    $ rebar3 compile

Tests
-----

    $ rebar3 ct

Browser Tests
-------------

The first step will be to add an hostfile entry on any client computer you
will use to test browsers, pointing the `snihost.custom` domain to the
computer on which the server is running (the self-signed certificate is
signed using that domain and it must be used to work).

First, make sure you know your server's IP (we aim for IPv4 here):

- If the server is public, you can find it [by googling 'what is my
  ip'](https://www.google.ca/search?q=parentheses+%28+%29&ie=utf-8&oe=utf-8&gws_rd=cr&ei=gpRcVZHsBsbfggSs34G4Dw#q=what+is+my+ip)
- if it's on your local network, you can use `ifconfig -a inet` and look for
  the address that is *not* on `lo0` (loopback, localhost) on OSX and Linux, or
  `ipconfig /all` on Windows, and look for the `IPv4 Address` entry noted with
  `(Preferred)` after it.
- If it's on the same computer, use `127.0.0.1`.

Then [find the host
file](http://en.wikipedia.org/wiki/Hosts_%28file%29#Location_in_the_file_system),
Add an entry in there saying:

    127.0.0.1 snihost.custom

(using whatever IP yours is rather than `127.0.0.1`). Everrything is set.
Start the `snit` server:

    $ rebar3 as demo shell
    1> application:ensure_all_started(snit).
    snit:start(http, 10, 8080, fun(_) -> [
        {certfile, "test/snit_basic_SUITE_data/selfsigned.crt"},
        {keyfile, "test/snit_basic_SUITE_data/selfsigned.key"}
    ] end, snit_http_hello, []).

Then open a browser to `https://snihost.custom:8080`. Do note that exceptions
will need to be added on browsers, and Chrome may never be happy with a custom
certificate on localhost if they require a CA chain that it can't resolve.
Self-signed certificates appear to be fine.

A connection can be debugged and inspected using the `openssl` debug client:

    $ openssl s_client -connect snihost.custom:8080 -tls1 -servername -state 2>&1

Configuration
-------------

To configure which ALPN Protocols Snit prefers, use your `sys.config`
or `application:set_env(snit, alpn_preferred_protocols, [<<"your protocol here">>])`.

It defaults to `[<<"http/1.1">>]`


Changelog
---------

Nothing's stable. Nor working.
