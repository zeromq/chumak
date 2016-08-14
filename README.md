erlangzmq 
=========

What is erlangzmq?
-------------------

`erlangzmq` is a library written in [Erlang](https://www.erlang.org/). It implements the ZeroMQ Message Transport Protocol (ZMTP). `erlangzmq` supports ZMTP version [3.1](http://rfc.zeromq.org/spec:37/ZMTP/).

Goal
----

The goal of `erlangzmq` application is to provide up-to-date native Erlang implementation of ZMTP.

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

Not implemented
--------------
1. CurveZMQ - security is not currently implemented. As a work-around, consider
using a proxy running [libzmq](https://github.com/zeromq/libzmq).

Install
-------

You can install `erlangzmq` from Hex: https://hex.pm/packages/erlangzmq or by referencing a specific tag directly:

```
{deps,[
  {erlangzmq, {git, "git@github.com:chovencorp/erlangzmq.git", {tag, "1.0.0"}}}  
]}.
```

For more info on rebar3 dependencies see the [rebar3 docs](http://www.rebar3.org/docs/dependencies).

Usage
-----

See [examples](https://github.com/chovencorp/erlangzmq/tree/master/examples). Otherwise use just like a regular Erlang/OTP application.

If you would like to use [python tests](https://github.com/chovencorp/erlangzmq/tree/master/python-test) to try language interop, you need to have [pyzmq](https://github.com/zeromq/pyzmq) installed. 

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

Copyright
----------
Copyright [Choven Corp.](http://choven.ca) 2016. All Rights Reserved.

FAQ
---
1. Why another Erlang implementation?

   Because the existing Erlang implementations and bindings are out of date.


2. Why a dual-license?

   To keep the code up-to-date. Having a commercial license allows us to charge money for code. Erlang is not a very
   trendy language, so the number of contributors to the open source project
   like this is small. In order to keep it from dying, we think it makes sense
   to run it as a commercial project.

3. Can I use `erlangzmq` for free?

   Yes, as long as you abide by the terms of the [AGPL license](COPYING.txt). In short, AGPL is a viral license,
   in that any code it touches has to be similarly licensed. So if you are working on an open-source project
   which has a [compatible](https://www.gnu.org/licenses/license-list.en.html) license, you can use `erlangzmq`. For more information see the [license](COPYING.txt) terms.

3. Why do I have to sign over my copyright when contributing?

   Short answer: for us to make money. Long answer: without your copyright, we could
   not dual-license the code, see "Why a dual-license?" above.


License
--------
erlangzmq is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

erlangzmq is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with erlangzmq.  If not, see <http://www.gnu.org/licenses/>

Commercial License
------------------

If you would like to use _erlangzmq_ without the restrictions of AGPL, please purchase a commercial license [here](http://choven.ca/#/softwaredev).

Acknowledgements
----------------
Our thanks to Wilson Júnior [Wpjunior](https://github.com/Wpjunior) for the great work he did on the initial
version of the codebase.

Contact
-------
You can contact a real human by emailing drozzy@choven.ca
