{ config, lib, pkgs, ... }:

let

  cfg = config.services.likely-music;

in {
  options.services.likely-music = {
    enable = lib.mkEnableOption "likely-music";
    virtualHost = lib.mkOption {
      type = lib.types.str;
      default = "localhost";
    };
    package = lib.mkOption {
      type = lib.types.package;
      default = (import ./. { inherit pkgs; }).likely-music;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.likely-music = {
      description = "likely-music web server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/likely-music";

        PrivateTmp = true;
        TemporaryFileSystem= "/:ro";
        BindReadOnlyPaths = "/nix";

        NoNewPrivileges = true;
        RestrictRealtime = true;
        LockPersonality = true;

        DynamicUser = true;

        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateUsers = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        ProtectKernelLogs = true;
        MemoryDenyWriteExecute = true;
        PrivateDevices = true;
        PrivateMounts = true;
      };
    };

    services.nginx.virtualHosts."${cfg.virtualHost}" = {
      enableACME = true;
      forceSSL = true;
      extraConfig = ''
        location / {
        proxy_pass http://localhost:8081/;
        }
      '';
    };
  };
}
