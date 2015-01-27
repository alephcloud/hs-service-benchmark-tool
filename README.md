---
title: service-benchmark-tools
layout: index
---

Usage
=====

Build and install the tool with

{% highlight bash %}
cabal install
{% endhighlight %}

For a simple example, essentially benchmarking the tool itself, first build and
run the simple test server (from `main/Main.hs`)

{% highlight bash %}
echos-service
{% endhighlight %}

and in another terminal run

{% highlight bash %}
service-benchmark-tool --thread-count=100 --action-count=1500 --url='http://localhost:8282/echo' --http-client=http-streams --loglevel=info
{% endhighlight %}

which will run 100 threads each making 1500 echo request to the echo-service
using the `http-streams` package as HTTP client backend and loglevel `info`.

For help you may type

{% highlight bash %}
service-benchmark-tool --help
{% endhighlight %}

The option `-p` can be used to print out the configuration as a configuration file and
replay a that configuration:

{% highlight bash %}
service-benchmark-tool --thread-count=100 -p > config.yml
service-benchmark-tool --config-file=config.yml
{% endhighlight %}

Installation
============

Installation with Chart Support
-------------------------------

Install chart-cairo:

{% highlight bash %}
cabal install alex
cabal install gtk2hs-buildtools
{% endhighlight %}

On Mac OS X with homebrew you must do

{% highlight bash %}
export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig:$PKG_CONFIG_PATH
{% endhighlight %}

and then

{% highlight bash %}
cabal install chart-cairo
{% endhighlight %}

Now you can install the package with:

{% highlight bash %}
cabal configure -fwith-chart
cabal install
{% endhighlight %}

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

Profiling
=========

{% highlight bash %}
cabal install --enable-profiling --ghc-option=-auto-all
{% endhighlight %}

Then run the program with `+RTS -prof` as show in this example:

{% highlight bash %}
./dist/build/service-benchmark-tool/service-benchmark-tool --thread-count=50 --action-count=1000 --url='http://127.0.0.1:8282' --http-client=http-client --loglevel=info +RTS -prof -N8
{% endhighlight %}

Note that there is a bug in GHC that causes the flag `-N` without argument to
have no effect with the threaded runtime.

ThreadScope
-----------

In order to collect event logs the application must be compiled with profiling
disabled:

{% highlight bash %}
cabal install --constraint='http-client>=0.4.7' --ghc-option='-eventlog' --ghc-option='-rtsopts' --disable-profiling --disable-library-profiling
{% endhighlight %}

Eventlogs can be obtained by running the application with +RTS -ls as show in
the following example:

{% highlight bash %}
 ./dist/build/service-benchmark-tool/service-benchmark-tool --thread-count=32 --action-count=1000 --url='http://127.0.0.1:8282' --http-client=http-client --loglevel=info -t 50000000  +RTS -ls -N8
{% endhighlight %}

This will result in a file called `service-benchmark-tool.eventlog` in the
working directory. This file can be opend and analyzed with ThreadScope.
