# chumak 

![Chumaki](docs/images/chumaki.jpg)

## What is chumak?

`chumak` is a library written in [Erlang](https://www.erlang.org/). It implements the ZeroMQ Message Transport Protocol (ZMTP). `chumak` supports ZMTP version [3.1](http://rfc.zeromq.org/spec:37/ZMTP/).

## Goal

The goal of `chumak` application is to provide up-to-date native Erlang implementation of ZMTP.

## Features

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


## Install

You can install `chumak` from [hex.pm](https://hex.pm/packages/chumak) by including the following in your `rebar.config`:

```erlang
{deps,[
	{chumak, "X.Y.Z"}
]}.
```
where _X.Y.Z_ is one of the [release versions](https://github.com/chovencorp/chumak/releases).

For more info on rebar3 dependencies see the [rebar3 docs](http://www.rebar3.org/docs/dependencies).

## Usage

See [examples](examples). Otherwise use just like a regular Erlang/OTP application.

If you would like to use [python tests](python-test) to try language interop, you need to have [pyzmq](https://github.com/zeromq/pyzmq) installed. 

## Build

```
$ rebar3 compile
```

By default, this will try to build a version of the application that
does not include support for the CURVE security model. 

The environment variable `CHUMAK_CURVE_LIB` can be used to specify a
NIF that implements the encryption functions that are required to support
the CURVE security model. 

The following values for `CHUMAK_CURVE_LIB` are supported:

- nacerl - this is the minimal variant using the tweetnacl C library. By
           default it is fetched and built from https://github.com/willemdj/NaCerl.  
           
           Compilation of nacerl requires gcc and make. Since these tools
           may not be available on windows systems, a check on the
           availability of these tools will be done. If they are not
           available the dependency will not be fetched and there will be
           no support for the CURVE security model.

- nacl   - this is similar to nacerl, but it depends on libsodium. The
           repository for this is https://github.com/tonyg/erlang-nacl. The
           the build process for Chumak will not automatically fetch and
           build it, but if `CHUMAK_CURVE_LIB` is set to "nacl", it will be
           assumed that this library is available and it will be used.

- enacl  - this also depends on libsodium, but it also requires 
           an Erlang VM that supports dirty schedulers. The repository is 
           https://github.com/jlouis/enacl. The build process for
           Chumak will not automatically fetch and build it, but if
           `CHUMAK_CURVE_LIB` is set to "enacl", it will be assumed that
           this library is available and it will be used.

## Test

```
$ rebar3 eunit -c
```
The `-c` will allow you to see the test coverage by running the command below.

## Coverage

```
$ rebar3 cover
```

## Generate Docs

```
$ rebar3 edoc
```

## Architecture

[Architecture](docs/architecture.md) describes the system structure.

## Help Wanted

Would you like to help with the project? Pick any of the issues tagged [help wanted](https://github.com/zeromq/chumak/labels/help%20wanted) and contribute!

## Contributing

See  [Contributing](CONTRIBUTING.md).


## FAQ

1. Why another Erlang implementation?

   Because the existing Erlang implementations and bindings are out of date.

2. Can I use `chumak` for free?

   Yes, as long as you abide by the terms of the [MPLv2 license](LICENSE). In short, you can include this code as a part of a larger work, even commercial. It is only when you modify `chumak` source code itself that you have to make that change available. Please read the license, as this description is not complete by any means.

3. Do I have to sign over my copyright when contributing?

   No. Everyone owns the piece of code they contribute.
   Please see [Contributing](CONTRIBUTING.md) for details.


## License

This project is licensed under Mozilla Public License Version 2.0.
See [license](LICENSE) for complete license terms.

## Etymology

From [Wikipedia](https://en.wikipedia.org/wiki/Chumak):

>Chumak (Ukrainian: чумак) is a historic occupation on the territory of the modern Ukraine 
>as merchants or traders, primarily known for the trade in salt.

## How to publish new Hex.pm version

To update a hex.pm version, you simply need to bump the version of erlang package and the github action will publish a new version on Hex.pm:

1. Adjust the version of the package in `src/chumak.app.src`
2. Commit & Push
3. Done



## How to publish to Hex.pm manually (Note: this is now superceeded by automatic github action above)

This info is here for maintainers - since I keep forgetting how to do this.

1. Adjust the version of the package in `src/chumak.app.src`
2. Login to hex.pm: `rebar3 hex user auth`
3. Put in your hex.pm username and your password (ignore the warning) - enter it 2 more times! (weird)
3. Publish: `rebar3 hex publish`
