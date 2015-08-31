0.1.1.31
--------
* Fix hardcoded encryption method

0.1.1.30
--------
* Add more encryption methods from OpenSSL
* Add a `--list-methods` flag to show all supported encryption methods

0.1.1.20
--------
* Implement TFO in remote. 

0.1.1.10
--------
* Add TCP Fast Open (TFO) capability. 

0.1.1.0
-------
* Use TCP_NOTSENT_LOWAT option to reduce latency

0.1.0.27
--------
* Make local respect `forbidden-ip` as well, this reduce unnecessary connections
  to the remote.
* Clean up logging.

0.1.0.26
--------
* Enable CIDR format in `forbidden-ip`.

0.1.0.25
--------
* Add `forbidden-ip` option, which defaults to `127.0.0.1`, to prevent misuse
  of remote. This is again a feature ported from ss.

0.1.0.24
--------
* Make obfuscation flush upper bound configurable in `config.json`.
* Fix an IPv6 name resolution bug in remote.

0.1.0.23
---------
* Fix a bug to prevent a half closed connection to hold a file handle.

0.1.0.22
--------
* Add the `-o` flag to turn on simple obfuscation (randomly flush socket to vary
  packet length). There is about a 10-20% performance cost.

0.1.0.21
--------
* Add in README that `GHC 7.10.2` is a must!

0.1.0.20
--------
* Rewrite Encrypt module, cache password hash.

0.1.0.19
--------
* Fix a memory leak.

0.1.0.18
--------
* Add complete command line arguments.

0.1.0.17
--------
* Add `--disable-socks5` flag to switch off the SOCKS5 service on local.

0.1.0.16
--------
* Add compatibility with ss for UDP port forwarding.

0.1.0.15
--------
* Add UDP port forwarding.

0.1.0.14
--------
* Add IPv6 support.
