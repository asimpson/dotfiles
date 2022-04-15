{ pkgs, ... }: {
  fileSystems."/MOUNT_NAME" = {
      device = "//IP/SHARE";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";

      in ["${automount_opts},credentials=/path/to/secret-file,uid=UID,gid=GID"];
  };
}
