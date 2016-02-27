{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.moesocks;
  configFile = pkgs.writeText "moesocks.json" (builtins.toJSON cfg);
  localIPs =
    '' 
      0.0.0.0/8
      10.0.0.0/8
      100.64.0.0/10
      127.0.0.0/8
      169.254.0.0/16
      172.16.0.0/12
      192.0.0.0/24
      192.0.2.0/24
      192.88.99.0/24
      192.168.0.0/16
      198.18.0.0/15
      198.51.100.0/24
      203.0.113.0/24
      224.0.0.0/4
      240.0.0.0/4
      255.255.255.255/32
      ::/128
      ::1/128
      ::ffff:0:0/96
      100::/64
      64:ff9b::/96
      2001::/32
      2001:10::/28
      2001:20::/28
      2001:db8::/32
      2002::/16
      fc00::/7
      fe80::/10
      ff00::/8
    '';
  aclFile = pkgs.writeText "deny.acl"
    ''
      ${optionalString cfg.forbidLocalIPs localIPs}

      ${cfg.extraDenyList}
    '';
in

{ options =
    { services.moesocks =
        { enable = mkEnableOption "moesocks SOCKS5 proxy server";

          verbose = mkOption {
            type = types.bool;
            default = false;
            description = "Turn on logging.";
          };

          role = mkOption {
            type = types.str;
            default = "local";
            description = "Tell moesocks to run as local or remote.";
          };

          tcp = mkOption {
            type = types.listOf types.str;
            default = [];
            example = [ "5300:8.8.8.8:53" ];
            description =
              ''
                Specify that the given TCP port on the local(client)
                host is to be forwarded to the given host and port on
                the remote side.
              '';
          };

          udp = mkOption {
            type = types.listOf types.str;
            default = [];
            example = [ "5300:8.8.8.8:53" ];
            description =
              ''
                Specify that the given UDP port on the local(client)
                host is to be forwarded to the given host and port on
                the remote side.
              '';
          };

          disableSOCKS5 = mkOption {
            type = types.bool;
            default = false;
            description =
              ''
                Do not start a SOCKS5 server on local. It can be
                useful to run moesocks only as a secure tunnel.
              '';
          };

          forbiddenIP = mkOption {
            type = types.listOf types.str;
            default = [];
            description = "IP list declared invalid as destinations";
          };

          remoteHost = mkOption {
            type = types.str;
            default = "::";
            description = "remote address";
          };

          remotePort = mkOption {
            type = types.int;
            default = 8388;
            description = "remote port";
          };

          localHost = mkOption {
            type = types.str;
            default = "::1";
            description = "local address";
          };

          localPort = mkOption {
            type = types.int;
            default = 1080;
            description = "local port";
          };

          timeout = mkOption {
            type = types.int;
            default = 3600;
            description = "timeout connection in seconds";
          };

          password = mkOption {
            type = types.str;
            default = "";
            description = "password";
          };

          method = mkOption {
            type = types.str;
            default = "aes-256-cfb";
            description = "encryption method";
          };

          fastOpen = mkOption {
            type = types.bool;
            default = false;
            description = "Use TCP_FASTOPEN, requires Linux 3.7+.";
          };

          forbidLocalIPs = mkOption {
            type = types.bool;
            default = true;
            description = "Do not proxy local IPs";
          };

          extraDenyList = mkOption {
            type = types.str;
            default = "";
            description = "Extra deny list";
          };
        };
    };

  config = mkIf cfg.enable {

    assertions =
      [
        { assertion = cfg.password != "";
          message = "moesocks' password must be set.";
        }
      ];

    users.extraUsers = singleton {
      name = "moesocks";
      # uid = config.ids.uids.moesocks;
      uid = 2000;
      description = "moesocks user";
    };

    systemd.services.moesocks =
      { wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        description = "moesocks SOCKS5 proxy server";
        serviceConfig =
          { User = "moesocks";
            ExecStart =
              ''
                ${pkgs.haskellPackages.moesocks}/bin/moesocks \
                  ${optionalString (cfg.verbose) "-v"} \
                  -r ${cfg.role} \
                  ${optionalString (cfg.tcp != []) "-T '${concatStringsSep " " cfg.tcp}'"} \
                  ${optionalString (cfg.udp != []) "-U '${concatStringsSep " " cfg.udp}'"} \
                  ${optionalString (cfg.disableSOCKS5) "--disable-socks5"} \
                  -c ${configFile} \
                  --deny-list ${aclFile}
              '';
          };
      };
  };
}
