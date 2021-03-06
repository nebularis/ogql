# Object Graph Qualification Language [![travis](https://secure.travis-ci.org/nebularis/ogql.png)](http://travis-ci.org/nebularis/ogql)

This library implements a parser for the query syntax described in the OGQL
specification, which can be found
[here](https://github.com/nebularis/ogql.spec).

## __Important Note__

The specification is currently under review and whilst this library is
undergoing refactoring to implement changes, the
[travis-build](http://travis-ci.org/nebularis/ogql) may be broken for a while.
Once the specification reaches version 0.1.0, this library will begin to mirror
the version increment semantics of the specification.

## License

The library is distributed under a permissive, BSD-like license.

## Building from source

You will need [Erlang/OTP](http://erlang.org) and a custom branch/fork of 
rebar, which can be found [here](https://github.com/hyperthunk/rebar/tree/econf).

    $ rebar -C init.config get-deps compile
    $ rebar -C build.config compile
    $ # run the samples...
    $ escript qt.erl

## Running the tests

Because of some odd behaviour as part of the build for
[delegate](https://github.com/hyperthunk/delegate), you will also need to set
the number of parallel build jobs to *1* in order to ensure a clean run.

    $ ./rebar -C init.config get-deps
    $ ./rebar -C test.config get-deps compile unittest -v jobs=1
