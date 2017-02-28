[![Build Status](https://magnum.travis-ci.com/heroku/snit.svg?token=uNxDxTYyzRaxPpJGQ5yq&branch=master)](https://magnum.travis-ci.com/heroku/snit)

snit
=====

Snit is Heroku's library for providing a basic, safe configuration for SNI terminating servers.

It provides a set of options that make it safe to set up a public-facing Erlang/OTP TLS server to the public. This work includes:

- Setting up a general security policy (based on Mozilla's and AWS) for cipher suites and their ordering
- Prioritizing ECCs to be performant and matching other industry players
- Disabling client-initiated renegotiations, and mandating secure one otherwise
- Restricting the set of allowed TLS/SSL versions (TLS 1.0 still allowed)

Build
-----

You'll need an Erlang version of 18.0 or newer. You can install one with
[`kerl`](https://github.com/yrashk/kerl) by calling:

    $ kerl build git https://github.com/erlang/otp.git OTP-19.1 OTP-19.1

Note that on OSX, you will need the newest cipher suites available.

    $ brew update
    $ brew upgrade openssl

Note the version you have (for example, `1.0.2a-1`) and tell `kerl` to use it:

    $ KERL_CONFIGURE_OPTIONS="--with-ssl=/usr/local/Cellar/openssl/1.0.2a-1" build git https://github.com/erlang/otp.git OTP-19.1 OTP-19.1

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

All configuration values are general to snit, and can be set through your `sys.config` file or through `application:set_env(snit, VarName, ValName)` for the more dynamic case.

- ALPN: use the variable `alpn_preferred_protocols` with a list of binary strings. Defaults to `[<<"http/1.1">>]`
- Cipher suites are configured with the `cipher_suites` option. The syntax is specific to be friendly both to people familiar with the OpenSSL notation and the internal Erlang one and more accurately described in [src/snit.app.src](src/snit.app.src#L13). Defaults to what should be a safe policy.
- ECCs selection and order are configured (in OTP-19.2 and later) through the `eccs` option. It accepts a list of atoms, and by default favors suites in order of 256 bits, 384 bits, and then from largest to smallest bit count. The objective there is to balance the speed of curves with security, where the default of secp256r1 is still generally a very good compromise. See the description for the configuration in [src/snit.app.src](src/snit.app.src#L84)

Contributing
------------

Contributing to this repository should be done through, generally, updating the configurations to line up with the Erlang/OTP ones that are made available, for the most secure configuration possible while remaining generally compatible with a wide group of users.

Tests should be accompanied by tests, and a strong rationale in the pull request as to why the new configuration ought to be safer than the existing one.

The patches will then be reviewed and double-checked before being merged. We do not necessarily expect to give very rapid responses since every default configuration change may be far-reaching and require careful validation.

On top of a strong rationale, we also expect:

- Tests
- Proper commit description
- Related documentation

Changelog
---------

- 1.0.0: open-source release
- 0.3.2: ECC curve selection
- ?.?.?: whole lot of stuff

Reference Material
------------------

- [AWS ELB security policy](http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-security-policy-table.html)
- [Mozilla's Security/Server side TLS](https://wiki.mozilla.org/Security/Server_Side_TLS)
- [TLS renegotiation and DoS](https://blog.qualys.com/ssllabs/2011/10/31/tls-renegotiation-and-denial-of-service-attacks)
- [Keys, PEM, DER, CSR, and CRL](http://serverfault.com/questions/9708/what-is-a-pem-file-and-how-does-it-differ-from-other-openssl-generated-key-file)
- Configuring Erlang Apps with SSL: [RabbitMQ](https://www.rabbitmq.com/ssl.html), [A blog post](https://terinstock.com/post/2014/07/TLS-with-Erlang/)
- [Generating self-signed keys](http://www.akadia.com/services/ssh_test_certificate.html)
- [Primer on SSL/TLS cache and session-reuse](https://vincent.bernat.im/en/blog/2011-ssl-session-reuse-rfc5077)
