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
  pdnsdUser = "pdnsd";
  pdnsdConf = pkgs.writeText "pdnsd.conf"
    ''
      global {
        run_as=${pdnsdUser};
        cache_dir="${cfg.cacheDir}";
        ${cfg.globalConfig}
      }

      server {
        ${cfg.serverConfig}
      }
      ${cfg.extraConfig}
    '';
in

{ options =
    { services.pdnsd =
        { enable = mkEnableOption "pdnsd";

          cacheDir = mkOption {
            type = types.str;
            default = "/var/cache/pdnsd";
            description = "Directory holding the pdnsd cache.";
          };

          globalConfig = mkOption {
            type = types.lines;
            default = "";
            description = ''
              Global configuration that should be added to the global directory
              of <literal>pdnsd.conf</literal>.
            '';
          };

          serverConfig = mkOption {
            type = types.lines;
            default = "";
            description = ''
              Server configuration that should be added to the server directory
              of <literal>pdnsd.conf</literal>.
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
      name = pdnsdUser;
      # uid = config.ids.uids.pdnsd;
      uid = 2010;
      description = "pdnsd user";
    };

    systemd.services.pdnsd =
      { wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        preStart =
          ''
            mkdir -p ${cfg.cacheDir}
            touch ${cfg.cacheDir}/pdnsd.cache
            chown -R ${pdnsdUser} ${cfg.cacheDir}
          '';
        description = "pdnsd";
        serviceConfig =
          {
            ExecStart = "${pdnsd}/bin/pdnsd -c ${pdnsdConf}";
          };
      };
  };
}
