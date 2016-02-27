{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.networking.moesocks-bento
; cleanIPv6 = x: removeSuffix "]" (removePrefix "[" x)
; isIPv6 = x: length (splitString ":" x) > 1
; cleanIP = x: if isIPv6 x then cleanIPv6 x else x
; socketAddress = address: port:
    if isIPv6 address
      then "[${address}]:${toString port}"
      else "${address}:${toString port}"

; cleanRemoteHost = cleanIP cfg.remoteHost
; useIPv6 = cfg.useIPv6
; localHost = if useIPv6 then "::1" else "127.0.0.1"
; localHostAllIPs = if useIPv6 then "::" else "0.0.0.0"
; sharedHost = x: if x then localHostAllIPs else localHost
; socks5LocalHost = sharedHost cfg.shareSOCKS5-Proxy
; dnsLocalHost = sharedHost cfg.shareDNS
; httpLocalHost = sharedHost cfg.shareHTTP-Proxy
; in

{

  imports =
    [
      ./moesocks.nix
      # ./pdnsd.nix
    ]

; options =
    {
      networking.moesocks-bento =
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

        ; remoteHost = mkOption
            { type = types.str
            ; default = ""
            ; description = "moesocks remote address"
            ; }


        ; shareDNS = mkOption
           { type = types.bool
           ; default = false
           ; description = "share the DNS server with other network users."
           ; }

        ; shareSOCKS5-Proxy = mkOption
           { type = types.bool
           ; default = false
           ; description = "share the SOCKS5 proxy server with other network users."
           ; }

        ; shareHTTP-Proxy = mkOption
           { type = types.bool
           ; default = false
           ; description = "share the HTTP proxy server with other network users."
           ; }

        ; useIPv6 = mkOption
          { type = types.bool
          ; default = true
          ; description = "whether to use the IPv6 stack"
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
            { type = types.str
            ; default = ""
            ; description = "moesocks password"
            ; }

        ; method = mkOption
            { type = types.str
            ; default = "aes-256-cfb"
            ; description = "encryption method"
            ; }

        ; }
    ; }

; config = mkIf cfg.enable
    { assertions =
        [
          { assertion = cfg.remoteHost != ""
          ; message = "moesocks' remote address must be set"
          ; }
        ]

    ; boot.kernel.sysctl."net.ipv4.tcp_fastopen" = 3

    ; services.nscd.enable = pkgs.lib.mkForce false

    ; services.moesocks =
        { enable = true
        ; tcp = [ "${toString cfg.dnsPort}:${cfg.remoteDNS}:53" ]
        ; remoteHost = cleanRemoteHost
        ; remotePort = cfg.remotePort
        ; localHost = socks5LocalHost
        ; localPort = cfg.socks5ProxyPort
        ; password = cfg.password
        ; method = cfg.method
        ; fastOpen = true
        ; }

    ; networking =
        { 
          proxy.default = "http://${socketAddress httpLocalHost cfg.httpProxyPort}"
        ; dhcpcd.extraConfig =
            ''
            nooption domain_name_servers
            nohook resolv.conf
            ''
        ; nameservers = [ localHost ]
        ; }

    ; services.privoxy =
        { enable = true
        ; listenAddress = socketAddress httpLocalHost cfg.httpProxyPort
        ; extraConfig = "forward-socks5 / ${socketAddress localHost cfg.socks5ProxyPort} ."
        ; }

    ; services.pdnsd =
        { enable = true
        ; globalConfig =
            ''
              perm_cache=4096;

              run_ipv4=${if useIPv6 then "off" else "on"};
              server_ip = "${dnsLocalHost}";
              status_ctl = on;

              neg_domain_pol=on;
              neg_rrs_pol=on;
              par_queries=1;

              query_method=tcp_only;
            ''
        ; serverConfig =
            ''
              label="proxy";
              ip = "${localHost}";
              port = ${toString cfg.dnsPort};

              purge_cache = off;

              proxy_only=on;

              exclude = .localdomain;
              exclude = .lan;
            ''
        ; extraConfig =
            ''
              source {
                owner=localhost;
                file="/etc/hosts";
              }
            ''
        ; }
    ; }

; }
