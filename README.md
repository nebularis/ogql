# Object Graph Qualification Language [![travis](https://secure.travis-ci.org/nebularis/ogql.png)](http://travis-ci.org/nebularis/ogql)

This library implements a parser for the query syntax described in the OGQL
specification, which can be found
[here](https://github.com/hyperthunk/eav_spec/wiki/OGQL-0.0.2).

## __Important Note__

The specification is currently under review and whilst this library is
undergoing refactoring to implement changes, the
[travis-build](http://travis-ci.org/nebularis/ogql) may be broken for a while.
Once the specification reaches version 0.1.0, this library will begin to mirror
the version increment semantics of the specification.

## License

The library is distributed under a permissive, BSD-like license.

## Building from source

You will need [Erlang/OTP](http://erlang.org) and
[rebar](https://github.com/basho/rebar) installed in order to
build the project.

    $ rebar get-deps compile
    $ rebar skip_deps=true compile
    $ # run the samples...
    $ chmod +x qt.erl
    $ ./qt.erl

## Running the tests

These require a custom branch/fork of rebar, which can be found
[here](https://github.com/hyperthunk/rebar/tree/econf).

    $ ./rebar -C init.config get-deps
    $ ./rebar -C test.config get-deps compile eunit -v
