# Object Graph Qualification Language.

This library implements a parser for the query syntax described in the OGQL
specification, which can be found
[here](https://github.com/hyperthunk/eav_spec/wiki/OGQL-0.0.2).

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

    $ rebar -C test.config get-deps compile eunit
