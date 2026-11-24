{
  modulesPath,
  lib,
  pkgs,
  ...
}:

let
  unstable = import (builtins.fetchTarball "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz") {
    config.allowUnfreePredicate = pkg: lib.getName pkg == "amp-cli";
    system = pkgs.stdenv.hostPlatform.system;
  };
  amp = unstable.amp-cli;
in
{
  imports = [
    "${modulesPath}/profiles/minimal.nix"
    "${modulesPath}/profiles/qemu-guest.nix"
    # root fs + bootloader, so nixos-rebuild switch works on a running vm too
    "${modulesPath}/virtualisation/disk-image.nix"
  ];

  networking = {
    hostName = "ampvm";
    useDHCP = true;
    firewall.enable = false;
  };

  programs.zsh.enable = true;
  programs.git.enable = true;

  services.qemuGuest.enable = true;

  services.tailscale = {
    enable = true;
    interfaceName = "userspace-networking";
    # tailscale ssh replaces openssh, incl. for make switch
    extraSetFlags = [ "--ssh" ];
  };

  users.mutableUsers = false;
  users.users.agent = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "docker"
    ];
    shell = pkgs.zsh;
    linger = true;
  };

  security.sudo.wheelNeedsPassword = false;
  # lets nixos-rebuild --target-host copy unsigned store paths in
  nix.settings.trusted-users = [ "@wheel" ];

  environment.systemPackages = with pkgs; [
    bashInteractive
    curl
    git
    htop
    btop
    jq
    ripgrep
    rsync
    vim
    gh
    fzf
    bat
    amp
  ];

  system.activationScripts.zshrc.text = ''
    cat > /home/agent/.zshrc <<'EOF'
    HISTFILE=/persist/zsh/history
    precmd() {
      PROMPT="%~ %{$fg[cyan]%}=vm=%{$reset_color%} "
      PROMPT+="%D{%I:%M}%{$fg[magenta]%}❯ %{$reset_color%}"
    }
    EOF
    chown agent:users /home/agent/.zshrc
  '';

  systemd.user.services.amp = {
    description = "Amp remote runner";
    wantedBy = [ "default.target" ];
    unitConfig.ConditionUser = "agent";
    path = [ "/run/current-system/sw" ];
    serviceConfig = {
      ExecStart = "${amp}/bin/amp --no-tui --runner-id ampvm --discover-dirs";
      Restart = "always";
      RestartSec = "5s";
    };
  };

  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
  };

  # on the host: qemu-img create -f qcow2 ~/ampvm-persist 1G
  # attach it to the vm in virtual machine manager
  # in the vm: mkfs.ext4 -L persist /dev/vdb
  fileSystems."/persist" = {
    device = "/dev/disk/by-label/persist";
    fsType = "ext4";
    neededForBoot = true;
  };

  systemd.tmpfiles.rules = [
    "d /persist/zsh 0700 agent users - -"
  ];

  system.stateVersion = "25.11";
}
