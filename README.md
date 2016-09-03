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
8. Error Handling
9. Framing
10. Socket-Type Property & Identity Property
11. Backwards Interoperability with ZMTP 3.0


Install
-------

You can install `chumak` from Hex: https://hex.pm/packages/chumak or by referencing a specific tag directly:

```
{deps,[
  {chumak, {git, "git@github.com:chovencorp/chumak.git", {tag, "1.0.0"}}}  
]}.
```

For more info on rebar3 dependencies see the [rebar3 docs](http://www.rebar3.org/docs/dependencies).

Usage
-----

See [examples](https://github.com/chovencorp/chumak/tree/master/examples). Otherwise use just like a regular Erlang/OTP application.

If you would like to use [python tests](https://github.com/chovencorp/chumak/tree/master/python-test) to try language interop, you need to have [pyzmq](https://github.com/zeromq/pyzmq) installed. 

Build
-----
```
$ rebar3 compile
```

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


Future work
------------
1. CurveZMQ - add security, with which chumak is compatible.

License
--------
This project is licensed under Mozilla Public License Version 2.0.
See [license](LICENSE) for complete license terms.

Etymology
---------
From [Wikipedia](https://en.wikipedia.org/wiki/Chumak):

>Chumak (Ukrainian: чумак) is a historic occupation on the territory of the modern Ukraine 
>as merchants or traders, primarily known for the trade in salt.