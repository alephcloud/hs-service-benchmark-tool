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

Scenarios
=========

We are interested in scenarios like:

> for 10000 enrolled users have each user login, read and email,
> send an email, and log out with some delay, such that at
> each time at about 1000 users are concurrently active.

This requires the following prerequesits:

1.  a large enough pool of enrolled users,
2.  for each user an email message (shall users share email message?), and
3.  for each user reciepients (are the recipients users that participate in the test?),

There are different ways to implement this:

1.  The email message for reading are pregenerated either

    1. for each user, or
    2. for groups of users,

2.  Consecutive users read the emails that were written by previous users,

3.  Users can be connected in different graphs, for instance as

    1.  as sets of isolated connected components,
    2.  as partial order,
    3.  as tree, or
    4.  as arbitrary graphs.

