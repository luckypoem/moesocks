0.1.0.24
--------
* Make obfuscation flush upper bound configurable in `config.json`
* Fix a IPv6 name resolution bug in remote

0.1.0.23
---------
* Fix a bug to prevent a half closed connection to hold a file handle

0.1.0.22
--------
* Add the `-o` flag to turn on simple obfuscation (randomly flush socket to vary
  packet length). There is about a 10-20% performance cost.

0.1.0.21
--------
* Add in README that `GHC 7.10.2` is a must!

0.1.0.20
--------
* Rewrite Encrypt module, cache password hash

0.1.0.19
--------
* Fix a memory leak

0.1.0.18
--------
* Add complete command line arguments

0.1.0.17
--------
* Add `--disable-socks5` flag to switch off the socks5 service on local

0.1.0.16
--------
* Add compatibility with shadowsocks for UDP port forwarding

0.1.0.15
--------
* Add UDP port forwarding

0.1.0.14
--------
* Add IPv6 support
