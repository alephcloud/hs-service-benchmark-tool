Performance of http-streams and http-client
===========================================

We compare the performance of the
[http-streams](https://hackage.haskell.org/package/http-streams) and the
[http-client](https://hackage.haskell.org/package/http-client) libraries. The
focus is on making a large number of small concurrent requests. This typically
occurs in services that use HTTP to connect to backend services, such as a
database, message queue, or log-message sink. Often those backend services use
relatively small JSON encoded messages.

Description and Results
-----------------------

Each benchmark initializes a number of threads. Each threads sequentially
performs the same number of identical synchronous HTTP requests to the same
HTTP server.

The results of some benchmarks can be viewed rendered as HTML here:
[https://rawgit.com/larskuhtz/d935b119f8b5790e2cda/raw/results.html](https://rawgit.com/larskuhtz/d935b119f8b5790e2cda/raw/results.html)

The following benchmarks are obtained with the [faster-timeout branch](https://github.com/snoyberg/http-client/tree/faster-timeout)
of the [http-client](https://hackage.haskell.org/package/http-client) package.
Note that all benchmarks are compiled with that branch. The Benchmarks that are labeled
`http-client-local-manager-no-timeout` use `managerResponseTimeout = Nothing` in `ManagerSettings`.

[https://rawgit.com/larskuhtz/d935b119f8b5790e2cda/raw/results-no-timeout.html](https://rawgit.com/larskuhtz/d935b119f8b5790e2cda/raw/results-no-timeout.html)

These results may indidcate the issue is not caused by the implementation of
`timeout` in `System.Timeout` but is sitting deeper somewhere in
`getSystemTimerManager` from `GHC.Event`. The latter is used by
`System.Timeout` as well as the timeout implementation the the
[faster-timeout branch](https://github.com/snoyberg/http-client/tree/faster-timeout) branch
of [http-client](https://hackage.haskell.org/package/http-client).

Setup
-----

The results are generated on a 4 core mac-book pro. The interest is on the
comparision of both libraries. No attempt has been made to reduce environment
noise or maximize performance.

The http-client library is benchmarked with two setups:

1.  Each thread receives it's own manager instance (connection pool) that keeps
    a single connection open.

2.  All threads share a common manager isntance (connection pool) that keeps as
    many connections open as there are threads.

The http-streams library is benchmarked with each thread allocating a single
static TCP connection. There is some logic in place to ensure that the
connection is reset in case an error occurs. This is not production level code,
but we think that a production grade implementation may have similar
performance. We implemented the following two setups:

1.  The TCP connection is stored in an `MVar`.

2.  The TCP connection is stored in an `IORef`.

The HTTP server is a [wai](https://hackage.haskell.org/package/wai) application
that just echos back the request body. It is running on
[warp](https://hackage.haskell.org/package/warp). For these results the server
with running in the same process as the benchmarks. We also run benchmarks
where the HTTP server was running as a different process on the same or another
machine. The results for those setups were similar to the results presented
here.

Code
----

The
[code](https://gist.github.com/larskuhtz/d935b119f8b5790e2cda#file-streamsvsclient-hs)
is compiled with

```.bash
ghc --make -O2 StreamsVsClient.hs -D'MIN_VERSION_http_streams(x,y,z)=1' -Wall -fforce-recomp -threaded
```

and executed as

```.bash
./StreamsVsClient -o results.html +RTS -N
```

