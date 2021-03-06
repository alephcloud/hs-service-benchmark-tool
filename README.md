[![Build Status](https://travis-ci.org/alephcloud/hs-service-benchmark-tool.svg)](https://travis-ci.org/alephcloud/hs-service-benchmark-tool)

[project site](http://alephcloud.github.io/hs-service-benchmark-tool/)

Usage
=====

Build and install the tool with

```.bash
cabal install
```

For a simple example, essentially benchmarking the tool itself, first build and
run the simple test server (from `main/Main.hs`)

```.bash
echos-service
```

and in another terminal run

```.bash
service-benchmark-tool --thread-count=100 --action-count=1500 --url='http://localhost:8282/echo' --http-client=http-streams --loglevel=info
```

which will run 100 threads each making 1500 echo request to the echo-service
using the `http-streams` package as HTTP client backend and loglevel `info`.

For help you may type

```.bash
service-benchmark-tool --help
```

The option `-p` can be used to print out the configuration as a configuration file and
replay a that configuration:

```.bash
service-benchmark-tool --thread-count=100 -p > config.yml
service-benchmark-tool --config-file=config.yml
```

Installation
============

Installation with Chart Support
-------------------------------

Install chart-cairo:

```.bash
cabal install alex
cabal install gtk2hs-buildtools
```

On Mac OS X with homebrew you must do

```.bash
export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig:$PKG_CONFIG_PATH
```

and then

```.bash
cabal install chart-cairo
```

Now you can install the package with:

```.bash
cabal configure -fwith-chart
cabal install
```

GHC-7.10
--------

For compilation with GHC-7.10 one has to install the `HEAD` versions of
`master` of `http-common` and `http-streams` from GitHub.

A build with `-fwith-chart` is not yet supported. You may also have to
explicitely pass `-f-old-local`.

Profiling
=========

```.bash
cabal install --enable-profiling --ghc-option=-auto-all
```

Then run the program with `+RTS -prof` as show in this example:

```.bash
./dist/build/service-benchmark-tool/service-benchmark-tool --thread-count=50 --action-count=1000 --url='http://127.0.0.1:8282' --http-client=http-client --loglevel=info +RTS -prof -N8
```

Note that there is a bug in GHC that causes the flag `-N` without argument to
have no effect with the threaded runtime.

ThreadScope
-----------

In order to collect event logs the application must be compiled with profiling
disabled:

```.bash
cabal install --constraint='http-client>=0.4.7' --ghc-option='-eventlog' --ghc-option='-rtsopts' --disable-profiling --disable-library-profiling
```

Eventlogs can be obtained by running the application with +RTS -ls as show in
the following example:

```.bash
 ./dist/build/service-benchmark-tool/service-benchmark-tool --thread-count=32 --action-count=1000 --url='http://127.0.0.1:8282' --http-client=http-client --loglevel=info -t 50000000  +RTS -ls -N8
```

This will result in a file called `service-benchmark-tool.eventlog` in the
working directory. This file can be opend and analyzed with ThreadScope.
