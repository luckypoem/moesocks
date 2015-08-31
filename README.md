MoeSocks
========

A SOCKS5 proxy using the client / server architecture.

MoeSocks is greatly inspired by [shadowsocks] and can be used in place of it.

Installation
============

* Need `GHC 7.10.2` and `cabal-install`.

  Installing [haskell-platform](https://www.haskell.org/platform/) should be
  sufficient. (Linux users should pick the *Generic* distribution, since
  it's the most up to date)

* Repeat, you need `GHC 7.10.2` exactly, not `7.10.1` or anything else, since
  remote only runs well in `7.10.2`.

* Update packages

        cabal update

* Install

        cabal install moesocks

* Add `~/.cabal/bin` to your `$PATH`, if you haven't already.

Usage
=====

* Download a sample [config.json] to your current path

* Edit `config.json` to fit your setup (at least the `remote` and `password`
  fields)

* Start a remote node outside a firewall:

        moesocks --role remote -c config.json

* Start a local node inside a firewall:

        moesocks --role local -c config.json

* Now you have a SOCKS5 proxy running inside a firewall using port
  `localPort`.

* Shadowsocks compatible obfuscation can be turned on with the `-o` flag to make
  statistical analysis on packet length a bit more confusing.

* See more options:

        moesocks --help

* You might want to run `moesocks` under some kind of a supervising daemon to
  auto restart the program if it crashes, likely due to [#10590], the fix of
  which was not included in the `7.10.2` release.


Features
========

* SOCKS5 proxy service, obviously
* TCP port forwarding
* UDP port forwarding, for example `-U 5300:8.8.8.8:53`
* TCP per connection throttling (as a side effect of trying to find a bug in the
remote)
* SOCKS5 service on local can be turned off
* Understand shadowsocks' configuration file

Drawbacks
==========

* UDP over SOCKS5 is not implemented
* More then 2 times slower then the original Python implementation (measured at
  20M/s vs 43M/s on an Intel P8800, using the AES-256-CFB method)


TCP Fast Open (TFO)
====================

## [TFO] means faster response in this case

Both local and remote will use TFO when instructed. If the browser in use and
the website to visit both support TFO, you can enjoy TFO all the way through.
This could lead to a huge reduction of latency.

## Enable TFO in your OS runtime.

On Linux 3.7+, to check the availability of TFO:

    cat /proc/sys/net/ipv4/tcp_fastopen

On Linux 3.7+, to enable TFO (as root):

    echo 3 > /proc/sys/net/ipv4/tcp_fastopen

## Enable TFO in MoeSocks

TFO can be turned on by adding a `"fastOpen":true` field in `config.json` or
specifying a `--fast-open` flag in the command line arguments.

## Verify

Use `tcpdump` on the `remotePort`, check for that `SYN` should start to carry
payload. An example command is:

    tcpdump port 8388 -i any -X -v


Credits
=======

* [shadowsocks] greatly inspired MoeSocks.
  Shadowsocks introduced a ground breaking design and implementation to bypass
  Internet censorship in China.
* [shadowsocks-haskell] by rnons, another implementation of shadowsocks in
  Haskell, also greatly inspired MoeSocks. Much of the understanding of
  the internal of shadowsocks was gained by reading rnons's implementation.

License
=======

Copyright 2015 Jinjing Wang

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

[shadowsocks]:https://github.com/shadowsocks/shadowsocks
[shadowsocks-go]:https://github.com/shadowsocks/shadowsocks-go
[shadowsocks-haskell]:https://github.com/rnons/shadowsocks-haskell
[config.json]:https://raw.githubusercontent.com/nfjinjing/moesocks/master/config.json
[#10590]:https://ghc.haskell.org/trac/ghc/ticket/10590
[TFO]:https://en.wikipedia.org/wiki/TCP_Fast_Open
