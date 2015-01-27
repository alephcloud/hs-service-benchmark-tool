---
title: service-benchmark
layout: index
---

service-benchmark-tool
======================

[README](README.html)

Benchmarks for System.Timeout
=============================

Results
:    [Timeout.html](test/Timeout/results.html)

Code
:    [Timeout.hs](https://github.com/alephcloud/hs-service-benchmark-tool/blob/master/test/Timeout/Timeout.hs)

These benchmarks evaluate the performance of `timeout` when used for many
short-running sequential actions from a moderate number of concurrent
threads.

In concrete the benchmarks spawn 16 concurrent threads and have each
sequentially repeat the same action 1000 times. The action consists of `n`
sequential calls to `getCurrentTime`. The benchmarks are executed for
different values for `n`.

We compare four different setups:

1. Just execute the basic setup precisly as described above.

2. Each action of `n` calls to `getCurrentTime` is wrapped into a call to
   `timeout`. The timeout parameter is chosen large enought so that it won't
   trigger.

3. Each action of `n` calls to `getCurrentTime` is started
   only after updating an `MVar` that is shared by all threads. The update
   is a cheap, but not yet trivial pure function.

4. Each action of `n` calls to `getCurrentTime` is started only after atomically
   updating an `IORef` that is shared by all threads. The update is a cheap,
   but not yet trivial pure function.

Note that the third and the fourth scenario force a synchronization between
*all* threads, whereas, to the best of my knowledge, the call to timeout
only requires synchronization between all threads on the same capability.

Usage:

{% highlight bash %}
cd test/Timeout
ghc --make -threaded Timeout.hs -rtsopts -O -fforce-recomp -Wall
./Timeout -o Timeout.html +RTS -N8
{% endhighlight %}

http-streams versus http-client
===============================

Results
:   [first results set](test/StreamsVsClient/results.html) and [second results set](test/StreamsVsClient/results-no-timeout.html)

Code
:   [StreamsVsClient.hs](https://github.com/alephcloud/hs-service-benchmark-tool/blob/master/test/StreamsVsClient/StreamsVsClient.hs)

README
:   [README.md](test/StreamsVsClient/README.md)

A more detailed description of the results is [published in this gist](https://gist.github.com/larskuhtz/d935b119f8b5790e2cda)
and discussed in [this issue of http-client](https://github.com/snoyberg/http-client/issues/98).

Usage:

{% highlight bash %}
cd test/StreamsVsClient
ghc --make -O2 StreamsVsClient.hs -Wall -fforce-recomp -threaded
./StreamsVsClient -o StreamsVsClient.html +RTS -N
{% endhighlight %}

