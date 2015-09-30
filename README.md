MoeSocks
========

A SOCKS5 proxy using the client / server architecture.

MoeSocks is greatly inspired by [ss] and can be used in place of it.

Installation
============

Easy
----

### Install [Nix]

    curl https://nixos.org/nix/install | sh 

### Install moesocks

    nix-env -iP -A nixpkgs.haskellPackages.moesocks

Hard
----

### Install GHC 7.10.2 and cabal-install
    
read manual

### Download moesocks

    git clone https://github.com/nfjinjing/moesocks

### Sandbox!

    cd moesocks
    cabal sandbox init

### Install

    cabal install

### Run

    .cabal-sandbox/bin/moesocks


Usage
=====

* Download a sample [config.json] to your current path

* Edit `config.json` to fit your setup (at least the `remoteHost` and `password`
  fields)

* Start a remote node outside a firewall:

        moesocks --role remote -c config.json

* Start a local node inside a firewall:

        moesocks --role local -c config.json

* Now you have a SOCKS5 proxy running inside a firewall using port
  `localPort`.

* SS compatible obfuscation can be turned on with the `-o` flag to make
  statistical analysis on packet length a bit more confusing.

* See more options:

        moesocks --help

* You might want to run `moesocks` under some kind of a supervising daemon to
  auto restart the program if it crashes, likely due to [#10590], the fix of
  which was not included in the `7.10.2` release.

* On `OSX`, If you only run `moesocks -r local`, then it should work. Occasional
  manual restart should be expected. `fastOpen` field should be `false` for now.


Features
========

* SOCKS5 proxy service, obviously
* TCP port forwarding
* UDP port forwarding, for example `-U 5300:8.8.8.8:53`
* TCP per connection throttling (as a side effect of trying to find a bug in the
remote)
* SOCKS5 service on local can be turned off
* Understand ss's configuration file

Drawbacks
==========

* UDP over SOCKS5 is not implemented.
* TCP bind over SOCKS5 is not implemented
* More then 2 times slower then the original Python implementation (measured at
  20M/s vs 43M/s on an Intel P8800, using the AES-256-CFB method, in software
  AES).
* Currently only works on Unix.


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

* [ss] greatly inspired MoeSocks.
* [ss-haskell] another implementation of ss in Haskell, also greatly inspired
  MoeSocks. Much of the understanding of the internal of ss was gained by
  reading ss-haskell.

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

[ss]:https://github.com/shadowsocks/shadowsocks
[ss-haskell]:https://github.com/rnons/shadowsocks-haskell
[Nix]:https://nixos.org/nix/
[config.json]:https://raw.githubusercontent.com/nfjinjing/moesocks/master/config.json
[#10590]:https://ghc.haskell.org/trac/ghc/ticket/10590
[TFO]:https://en.wikipedia.org/wiki/TCP_Fast_Open
[ECN]:https://en.wikipedia.org/wiki/Explicit_Congestion_Notification

