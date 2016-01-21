{ config, pkgs, lib, ... }:

with lib;

let cfg = config.services.moesocks; in

{ options =
    { services.moesocks =
       { enable = mkOption
           { type = types.bool
           ; default = false
           ; description = "Whether to run the moesocks socks5 proxy"
           ; }

       ; tcp = mkOption
           { type = types.listOf types.str
           ; default = []
           ; example = [ "5300:8.8.8.8:53" ]
           ; description =
               ''
                 Specify that the given TCP port on the local(client)
                 host is to be forwarded to the given host and port on
                 the remote side.
               ''
           ; }

        ; remote = mkOption
            { type = types.str
            ; default = "::"
            ; description = "remote address"
            ; }

        ; remotePort = mkOption
            { type = types.int
            ; default = 8388
            ; description = "remote port"
            ; }

        ; local = mkOption
            { type = types.str
            # Default to listening on an IPv6 localhost address since otherwise
            # initial start of moesocks will mysteriously fail.
            ; default = "::1"
            ; description = "local address"
            ; }

        ; localPort = mkOption
            { type = types.int
            ; default = 1080
            ; description = "local port"
            ; }

        ; timeout = mkOption
            { type = types.int
            ; default = 3600
            ; description = "timeout connection in seconds"
            ; }

        ; password = mkOption
            { type = types.str
            ; default = "birthday!"
            ; description = "password"
            ; }

        ; method = mkOption
            { type = types.str
            ; default = "aes-256-cfb"
            ; description = "encryption method"
            ; }

        ; fastOpen = mkOption
            { type = types.bool
            ; default = false
            ; description = "Use TCP_FASTOPEN, requires Linux 3.7+"
            ; }

        ; }
    ; }

; config = mkIf cfg.enable
    { users.extraUsers = singleton
      { name = "moesocks"
      ; uid = 2000 # config.ids.uids.moesocks
      ; description = "moesocks dummy user"
      ; }

    ; systemd.services.moesocks =
      { wantedBy = [ "multi-user.target" ]
      ; after = [ "network.target" ]
      ; description = "moesocks socks5 local proxy"
      ; serviceConfig =
          { User = "moesocks"
          ; ExecStart =
          ''
             ${pkgs.haskellPackages.moesocks}/bin/moesocks -s ${cfg.remote} -p ${toString cfg.remotePort} -b ${cfg.local} -l ${toString cfg.localPort} -t ${toString cfg.timeout} -k ${cfg.password} -T ${concatStringsSep " " cfg.tcp} -m ${cfg.method} ${optionalString (cfg.fastOpen) "--fast-open"}
          ''
          ; }
      ; }
    ; }


; }
