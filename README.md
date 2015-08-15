MoeSocks
========


A socks5 proxy using the client / server architecture.

MoeSocks is greatly inspired by [shadowsocks].

A sample `config.json` file is included in this repository and the cabal
archive.

type `moesocks --help` for help.

Usage
-----

* Start a remote node outside the firewall: 
          
        moesocks -m remote -c config.json
* Start a local node inside the firewall: 
    
        moesocks -m local -c config.json
* Now you have a socks5 proxy running inside the firewall using port 
  `localPort`.
* The above commands can be shortened to: `moesocks -m remote` and `moesocks`.

Features
--------
* TCP port forwarding
* UDP port forwarding, for example `-U 5300:8.8.8.8:53`
* TCP per connection throttling (as a side effect of trying to find a bug in the
remote)
* Disable socks5 service on local

Not working
-----------
* Remote is flaky 
* UDP over Socks5 is not implemented
* 2.5 times slower then the original Python implementation (measured at 17.6M/s
    vs 43.6M/s on an Intel P8800, using the AES-256-CFB encryption)

Planning features
------------------
* None

Note
------

There's a bug that prevents remote from working correctly.

You should use the original Python implementation of [shadowsocks] on the remote
server.

There is an earlier implementation of [shadowsocks in Haskell] by rnons that
makes MoeSocks possible. 

The original goal of MoeSocks is to provide extra configurability to standard
shadowsocks, but it has been discarded since the remote is too flaky. 

[shadowsocks]:https://github.com/shadowsocks/shadowsocks 
[shadowsocks in Haskell]:https://github.com/rnons/shadowsocks-haskell



