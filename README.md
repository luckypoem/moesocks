MoeSocks
========

A socks5 proxy using the client / server architecture.

MoeSocks is greatly inspired by [shadowsocks] and can be used in place of it.

Installation
------------

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
-----
* Download a sample [config.json] to your current path

* Edit `config.json` to fit your setup (at least the `remote` and `password`
  fields)

* Start a remote node outside the firewall: 
          
        moesocks --role remote -c config.json

* Start a local node inside the firewall: 
    
        moesocks --role local -c config.json

* Now you have a socks5 proxy running inside the firewall using port 
  `localPort`.

* See more options:

        moesocks --help


Features
--------
* Socks5 proxy service, obviously
* TCP port forwarding
* UDP port forwarding, for example `-U 5300:8.8.8.8:53`
* TCP per connection throttling (as a side effect of trying to find a bug in the
remote)
* Socks5 service on local can be turned off
* Understand shadowsocks' configuration file

Not working
-----------
* UDP over Socks5 is not implemented
* More then 2 times slower then the original Python implementation (measured at
  20M/s vs 43M/s on an Intel P8800, using the AES-256-CFB method)

Planning features
------------------
* None

Credits
-------
* [shadowsocks] greatly inspired MoeSocks.
  Shadowsocks introduced a ground breaking design and implementation to bypass
  Internet censorship in China. What's unique about it is that instead of
  relying on a persistent TCP connection to transmit all data, it uses the
  design of a socks proxy, which start a new connection for every request, but
  without using a protocol header. Key exchange is done offline and there is no
  handshake anywhere inside a transmission. Deep packet inspection (DPI) becomes
  infeasible unless AES256 stream cipher can be broken on a per connection
  basis. This is made more difficult by that the encryption method can be easily
  swapped to another one, for example Salsa20. In fact, every shadowsocks user
  is likely to be using an encryption method that please them the most. With
  this much redundancy in encryption options, the Great Firewall of China (GFW)
  is likely to be left with only two options: block any transmission it can not
  understand using any method, particularly DPI, or leave shadowsocks alone. So
  far GFW has chosen the latter. Can you see the genius design of shadowsocks
  now?
* [shadowsocks-haskell] by rnons, another implementation of shadowsocks in 
  Haskell, also greatly inspired MoeSocks. Much of the understanding of
  the internal of shadowsocks was gained by reading rnons's implementation.

License
--------
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


