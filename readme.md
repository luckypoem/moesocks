MoeSocks
========


A socks5 proxy using the client / server architecture.

MoeSocks is greatly inspired by [shadowsocks].

A sample `config.json` file is included in this repository and the cabal
archive.

type `moesocks --help` for help.

Features
--------
* GFW compatibility
* Can be used as a drop in replacement for shadowsocks (only client mode)
* TCP port forwarding 

Planning features
------------------
* UDP socks5 proxy
* UDP port forwarding 
* Web based monitoring and profiling

Note
------

You should use the python implementation of [shadowsocks] on the remote
server, since the remote mode of moesocks is still buggy :(

There is an earlier implementation of [shadowsocks in Haskell] by rnons. 

The goal of moesocks is to provide extra configurability to standard
shadowsocks, for example:

* load balancing (region based multi-server configuration) IP level request
* filters (like iptables)

[shadowsocks]:https://github.com/shadowsocks/shadowsocks 
[shadowsocks in Haskell]:https://github.com/rnons/shadowsocks-haskell



