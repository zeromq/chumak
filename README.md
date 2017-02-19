chumak 
=========

![Chumaki](docs/images/chumaki.jpg)

What is chumak?
-------------------

`chumak` is a library written in [Erlang](https://www.erlang.org/). It implements the ZeroMQ Message Transport Protocol (ZMTP). `chumak` supports ZMTP version [3.1](http://rfc.zeromq.org/spec:37/ZMTP/).

Goal
----

The goal of `chumak` application is to provide up-to-date native Erlang implementation of ZMTP.

Features
--------
1. Resource Property *(NEW in 3.1!)*
2. Request-Reply pattern
3. Publish-Subscribe pattern
4. Pipeline Pattern	
5. Exclusive Pair Pattern
6. Version Negotiation
7. NULL Security Mechanism
8. CURVE Security Mechanism
9. Error Handling
10. Framing
11. Socket-Type Property & Identity Property
12. Backwards Interoperability with ZMTP 3.0


Install
-------

You can install `chumak` from [hex.pm](https://hex.pm/packages/chumak) by including the following in your `rebar.config`:

```
{deps,[
	{chumak, "X.Y.Z"}
]}.
```
where _X.Y.Z_ is one of the [release versions](https://github.com/chovencorp/chumak/releases).

For more info on rebar3 dependencies see the [rebar3 docs](http://www.rebar3.org/docs/dependencies).

Usage
-----

See [examples](examples). Otherwise use just like a regular Erlang/OTP application.

If you would like to use [python tests](python-test) to try language interop, you need to have [pyzmq](https://github.com/zeromq/pyzmq) installed. 

Build
-----
```
$ rebar3 compile
```

By default, this will try to build a version of the application that
includes support for the CURVE security model. For this it needs 
a NIF that handles the cryptographic functions. The rebar3
configuration has been set up so that 
[nacerl](https://github.com/willemdj/NaCerl) will be fetched and built as a
dependency. 

Compilation of nacerl requires gcc and make. Since these tools may not be
available on windows systems, a check on the availability of these tools
will be done. If they are not available the dependency will
not be fetched and there will be no support for the CURVE
security model.

Other options for the cryptographic functions are
[enacl](https://github.com/jlouis/enacl) and
[erlang-nacl](https://github.com/tonyg/erlang-nacl). Since it is not
feasible to build these options completely using rebar3, they will have to
be installed using other means. Chumak can be built in such a way
that one of these other libraries is used by setting the environment
variable `CHUMAK_CURVE_LIB`.

The following values for `CHUMAK_CURVE_LIB` are supported:

- nacerl - this is the minimal variant using the tweetnacl C library. By
           default it is fetched and built from https://github.com/willemdj/NaCerl.

- nacl   - this is similar to nacerl, but it depends on libsodium. The
           repository for this is https://github.com/tonyg/erlang-nacl. The
           the build process for Chumak will not automatically fetch and
           build it.

- enacl  - this also depends on libsodium, but it also requires 
           an Erlang VM that supports dirty schedulers. The repository is 
           https://github.com/jlouis/enacl. The build process for
           Chumak will not automatically fetch and build it.

- none   - no support for the CURVE security model. In this case no
           dependency will be fetched and any attempt to use the CURVE
           model will result in an error.

Test
----
```
$ rebar3 eunit -c
```
The `-c` will allow you to see the test coverage by running the command below.

Coverage
--------
```
$ rebar3 cover
```

Generate Docs
-------------
```
$ rebar3 edoc
```

Architecture
-------------
[Architecture](docs/architecture.md) describes the system structure.

Contributing
------------

See  [Contributing](CONTRIBUTING.md).


FAQ
---
1. Why another Erlang implementation?

   Because the existing Erlang implementations and bindings are out of date.

2. Can I use `chumak` for free?

   Yes, as long as you abide by the terms of the [MPLv2 license](LICENSE). In short, you can include this code as a part of a larger work, even commercial. It is only when you modify `chumak` source code itself that you have to make that change available. Please read the license, as this description is not complete by any means.

3. Do I have to sign over my copyright when contributing?

   No. Everyone owns the piece of code they contribute.
   Please see [Contributing](CONTRIBUTING.md) for details.


License
--------
This project is licensed under Mozilla Public License Version 2.0.
See [license](LICENSE) for complete license terms.

Etymology
---------
From [Wikipedia](https://en.wikipedia.org/wiki/Chumak):

>Chumak (Ukrainian: чумак) is a historic occupation on the territory of the modern Ukraine 
>as merchants or traders, primarily known for the trade in salt.
