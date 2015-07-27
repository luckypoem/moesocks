MoeSocks
========


A socks5 proxy using the client / server architecture.

MoeSocks is greatly inspired by [shadowsocks].

A sample `config.json` file is included in this repository and the cabal archive.

type `moesocks --help` for help.

Notes
------

There is still a bug in remote mode which makes flaky connections. Client mode
is usable.

There is an earlier implementation of [shadowsocks in Haskell] by rnons. 

The goal of moesocks is to provide extra configurability to standard
shadowsocks, for example:

* load balancing (for example, region based multi-server configuration)
* IP level request filters (like iptables)

[shadowsocks]:https://github.com/shadowsocks/shadowsocks
[shadowsocks in Haskell]:https://github.com/rnons/shadowsocks-haskell



