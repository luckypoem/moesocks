{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.networking.moesocksBento
; in

{
  imports = [ ./moesocks.nix ]

; options =
    {
      networking.moesocksBento =
        { enable = mkEnableOption "moesocks bento proxy suite"

        ; socks5ProxyPort = mkOption
            { type = types.int
            ; default = 1080
            ; description = "port for the SOCKS5 proxy server"
            ; }

        ; httpProxyPort = mkOption
            { type = types.int
            ; default = 8118
            ; description = "port for the HTTP proxy server"
            ; }

        ; dnsPort = mkOption
           { type = types.int
           ; default = 5300
           ; description = "port for tunneling DNS"
           ; }

        ; remote = mkOption
            { type = types.str
            ; default = ""
            ; description = "moesocks remote address"
            ; }

        ; local = mkOption
            { type = types.str
            ; default = "[::1]"
            ; description = "local address"
            ; }

        ; remotePort = mkOption
            { type = types.int
            ; default = 8388
            ; description = "moesocks remote port"
            ; }

        ; remoteDNS = mkOption
            { type = types.str
            ; default = "8.8.8.8"
            ; description = "remote DNS address"
            ; }

        ; password = mkOption
            {  type = types.str
            ; default = ""
            ; description = "moesocks password"
            ; }
        ; }
    ; }

; config = mkIf cfg.enable
    { assertions =
        [
          { assertion = cfg.remote != ""
          ; message = "moesocks' remote address must be set"
          ; }
        ]

    ; boot.kernel.sysctl."net.ipv4.tcp_fastopen" = 3

    ; services.nscd.enable = pkgs.lib.mkForce false

    ; services.moesocks =
        { enable = true
        ; udp = [ "${toString cfg.dnsPort}:${cfg.remoteDNS}:53" ]
        ; remote = cfg.remote
        ; remotePort = cfg.remotePort
        ; localPort = cfg.socks5ProxyPort
        ; password = cfg.password
        ; fastOpen = true
        ; }

    ; networking =
        { proxy.default = "socks5://${cfg.local}:${toString cfg.socks5ProxyPort}"
        ; dhcpcd.extraConfig =
            ''
            nooption domain_name_servers
            nohook resolv.conf
            ''
        ; }

    ; services.privoxy =
        { enable = true
        ; listenAddress = "${cfg.local}:${toString cfg.httpProxyPort}"
        ; extraConfig = "forward-socks5 / ${cfg.local}:${toString cfg.socks5ProxyPort} ."
        ; }

    ; services.dnsmasq =
        let dnsmasqHost = removeSuffix "]" (removePrefix "[" cfg.local)
        ; in

        { enable = true
        ; servers = [ "${dnsmasqHost}#${toString cfg.dnsPort}" ]
        ; }

    ; }

; }
