MoeSocks
========

A SOCKS5 proxy using the client / server architecture.

MoeSocks is compatible with [ss] at the protocal and most of the configuration level.

Installation
============

From binary
-----------

### Install [Nix]

    curl https://nixos.org/nix/install | sh 

### Install moesocks

    nix-env -i -A nixpkgs.haskellPackages.moesocks

### Run

    moesocks 

From source
-----------

### Install GHC and cabal-install
    
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

* Start a remote node outside a firewall:

        moesocks -r remote -k birthday!

* Start a local node inside a firewall:

        moesocks -s $REMOTE_IP -k birthday!

* Now you have a SOCKS5 proxy running inside a firewall on port `1080`.

* See more options:

        moesocks --help

* Not tested in `OSX`, but it should just work.

System wide proxy for NixOS
===========================

* Copy `nixos/proxy` to `/etc/nixos/proxy` 
* Modify the following and add it to `configuration.nix`:

        imports = [ ./proxy/moesocks-bento.nix ];
  
        networking.moesocks-bento =
          {
            enable = true;
            remoteHost = "my-server";
            remotePort = 8388;
            password = "birthday!";
          };

Features
========

* SOCKS5 proxy service
* TCP port forwarding
* UDP port forwarding, for example to tunnel DNS request: `-U 5300:8.8.8.8:53`
* SOCKS5 service on local can be turned off
* Understand `ss`' configuration file

Known issues
============

* UDP over SOCKS5 is not implemented.
* TCP bind over SOCKS5 is not implemented
* More then 2 times slower then the original Python implementation (measured at
  20M/s vs 43M/s on an Intel P8800, using the AES-256-CFB method, in software
  AES).
* Currently only works on Unix.


TCP Fast Open (TFO)
====================

## Benefit of using [TFO]

TFO can bypass the TCP three-way handshake in successive connections, thus reducing latency.

## Enable TFO in your OS runtime.

On Linux 3.7+, to check the availability of TFO:

    cat /proc/sys/net/ipv4/tcp_fastopen

On Linux 3.7+, to enable TFO (as root):

    echo 3 > /proc/sys/net/ipv4/tcp_fastopen

## Enable TFO in MoeSocks

TFO can be turned on by adding a `"fastOpen":true` field in `config.json` or
adding a `--fast-open` argument in the command line.

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

