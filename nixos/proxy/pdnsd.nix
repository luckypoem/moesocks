{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.pdnsd;
  pdnsd = pkgs.lib.overrideDerivation pkgs.pdnsd
           (attrs:
             {
               configureFlags =
                 [ "--enable-ipv6"
                 ];
             }
           );

  cacheDir = "/var/cache/pdnsd";
  pdnsdConf = pkgs.writeText "pdnsd.conf"
    ''
      global {
        run_as="pdnsd";
        ${cfg.globalConfig}
      }

      ${cfg.extraConfig}
    '';
in

{ options =
    { services.pdnsd =
        { enable = mkEnableOption "pdnsd";

          globalConfig = mkOption {
            type = types.lines;
            default = "";
            description = ''
              Global configuration that should be added to
              the global directory of 
              <literal>pdnsd.conf</literal>.
            '';
          };

          extraConfig = mkOption {
            type = types.lines;
            default = "";
            description = ''
              Extra configuration directives that should be added to
              <literal>pdnsd.conf</literal>.
            '';
          };
        };
    };

  config = mkIf cfg.enable {
    users.extraUsers = singleton {
      name = "pdnsd";
      # uid = config.ids.uids.pdnsd;
      uid = 2010;
      description = "pdnsd user";
    };

    systemd.services.pdnsd =
      { wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        preStart =
          ''
            mkdir -p ${cacheDir}
            touch ${cacheDir}/pdnsd.cache
            chown -R pdnsd ${cacheDir}
          '';
        description = "pdnsd";
        serviceConfig =
          {
            # pdnsd expects the user starting the daemon to own the
            # configuration file
            User = "root"; 
            # PermissionsStartOnly = true;
            ExecStart = "${pdnsd}/bin/pdnsd -c ${pdnsdConf}";
          };
      };
  };
}
