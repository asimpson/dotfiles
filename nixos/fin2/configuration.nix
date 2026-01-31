{ config, lib, pkgs, ... }:

let
  shared = import ../shared.nix;
  kolide-key = import ../kolide-key.nix;
  hosts = import ./hosts.nix;
  rofi-clipster = import ../local-packages/rofi-clipster.nix { inherit pkgs; };
  flake-compat = import (fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz");
  ghostty-flake = flake-compat {
    src = fetchTarball "https://github.com/ghostty-org/ghostty/archive/main.tar.gz";
  };
  kolide-flake = flake-compat {
    src = fetchTarball "https://github.com/kolide/nix-agent/archive/main.tar.gz";
  };
in
{
  imports = [ ./hardware-configuration.nix ../scripts.nix ../fin/packages.nix ../display-config/1440p.nix kolide-flake.defaultNix.nixosModules.kolide-launcher ];

#  specialisation."fix-amd-crash".configuration = {
#    system.nixos.tags = [ "old-firmware" ];
#
#    hardware.firmware = [
#      (pkgs.linux-firmware.overrideAttrs (old: {
#        version = "20251111";
#        src = pkgs.fetchurl {
#          url = "https://www.kernel.org/pub/linux/kernel/firmware/linux-firmware-20251111.tar.gz";
#          sha256 = "0rp2ah8drcnl7fh9vbawa8p8c9lhvn1d8zkl48ckj20vba0maz2g";
#        };
#      }))
#    ];
#  };

  environment.etc."kolide-k2/secret" = {
    mode = "0600";
    text = kolide-key.kolide-key;
  };

  environment.systemPackages = with pkgs; [
    _1password-gui
    zoom-us
    vscode
    nixfmt-rfc-style
    rofi-clipster
    feh
    pulseaudio
    bat
    lxqt.lxqt-policykit
    impala
    ghostty-flake.defaultNix.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];

  system.activationScripts.binbash = {
    text = ''
      if [ ! -f "/bin/bash" ]; then
        ln -sf ${pkgs.bash}/bin/bash /bin/bash
      fi
    '';
  };

  boot = {
    supportedFilesystems = [ "zfs" ];

    # Bootloader
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    # v4l2loopback for virtual camera
    extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];

    kernelModules = [ "v4l2loopback" "kvm-amd" "corefreqk" "sg" ];

    kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
      "net.ipv6.conf.all.forwarding" = 1;
    };
  };

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [
    (self: super: {
      rofi = super.rofi.override {
        plugins = [ super.rofi-mpd super.rofi-emoji super.rofi-calc ];
      };
    })
  ];

  services.playerctld.enable = true;

  systemd = {
    user = {
      services = {
        clipster = {
          enable = true;
          description = "clipster clipboard manager daemon";
          partOf = [ "graphical-session.target" ];
          after = [ "graphical-session-pre.target" ];
          wantedBy = [ "graphical-session.target" ];
          serviceConfig = {
            ExecStart = "${pkgs.clipster}/bin/clipster -d";
            Restart = "always";
          };
        };
      };
    };
    services = {
      backupmail = {
        path = [
          pkgs.age
        ];
        script = ''
          set -eu
          ${pkgs.getmail6}/bin/getmail
          ${pkgs.notmuch}/bin/notmuch new
        '';
        serviceConfig = {
          User = "adam";
        };
        startAt = "hourly";
      };
    };
  };

  time.timeZone = "America/New_York";

  nix = {
    settings = {
      auto-optimise-store = true;
      allowed-users = [ "@wheel" ];
      trusted-substituters = [
        "file:///var/nix-cache"
      ];
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  security.polkit.enable = true;
  programs.dconf.enable = true;
  programs.zsh.enable = true;
  programs.ssh.startAgent = true;
  programs.light.enable = true;
  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = [ "adam" ];
  };

  security.pam = {
    u2f.enable = true;
    u2f.settings.cue = true;
    u2f.settings.authFile = "/home/adam/.config/Yubico/u2f_keys";

    services = {
      login.u2fAuth = true;
      login.fprintAuth = false;
      lightdm.u2fAuth = true;
      sudo.u2fAuth = true;
      sudo.fprintAuth = false;
      lightdm.enableGnomeKeyring = true;
      login.enableGnomeKeyring = true;
    };
  };

  security.pki.certificates = [
    shared.local_cert
  ];

  networking = {
    useNetworkd = true;
    useDHCP = false;
    extraHosts = hosts.hosts;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 3000 8080 ];
      allowedUDPPorts = [ 41641 ];
      checkReversePath = "loose";
      extraCommands = hosts.extra;
    };
    hostName = "fin2";
    hostId = "5a488f19";
    iproute2.enable = true;
    wireless = { iwd = { enable = true; }; };
  };

  systemd.network = {
    enable = true;

    links."10-wan" = {
      matchConfig.MACAddress = "9c:6b:00:9a:f7:29";
      linkConfig = {
        Name = "wan";
        WakeOnLan = "magic";
      };
    };

    networks."20-wan" = {
      matchConfig.Name = "wan";
      networkConfig = {
        DHCP = "yes";
      };
      dhcpV4Config = {
        RouteMetric = 100;
      };
    };
  };

  fonts = {
    enableDefaultPackages = true;
    # Give fonts to 32-bit binaries too (e.g. steam).
    fontconfig.cache32Bit = true;
    packages = with pkgs; [
      hack-font
      google-fonts
      liberation_ttf
      open-sans
      roboto
      roboto-mono
    ];
  };
  hardware.logitech.wireless.enable = true;

  # Docker configuration
  virtualisation = {
    spiceUSBRedirection.enable = true;
    libvirtd = {
      enable = true;

      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;
      };
    };
    docker = {
      enable = true;
      autoPrune.enable = true;
    };
  };

  services = {
    kolide-launcher.enable = true;
    sunshine = {
      enable = true;
      capSysAdmin = true;
    };
    picom = {
      enable = true;
      vSync = true;
      backend = "glx";
    };
    timesyncd = {
      enable = true;
      servers = [ "time.google.com" ];
    };
    fwupd.enable = true;
    cron.enable = true;
    udev = {
      extraRules = ''
        LABEL="gmk pro regular user access"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="320f", ATTRS{idProduct}=="5044", TAG+="uaccess"
      '';
    };
    xserver = {
      xkb = { layout = "us"; };
      enable = true;
      desktopManager = { xterm.enable = false; };

      windowManager.i3 = {
        enable = true;
        extraPackages = with pkgs; [
          i3status # gives you the default i3 status bar
        ];
      };
    };
    displayManager = { defaultSession = "none+i3"; };
    openssh = {
      enable = true;
      ports = [ 2222 ];
    };
    tailscale.enable = true;
    resolved.enable = true;
    gnome.sushi.enable = true;
    gnome.gcr-ssh-agent.enable = false; #https://search.nixos.org/options?channel=25.11&show=services.gnome.gcr-ssh-agent.enable&query=services.gnome.gcr-ssh-agent.enable
    printing = {
      enable = true;
      drivers = [ pkgs.brlaser ];
    };
    avahi = {
      enable = true;
      nssmdns4 = true;
    };
    gnome.gnome-keyring.enable = true;
  };

  users.users.adam = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "audio" "video" "libvirtd" "lp" "wheel" "docker" ];
  };

  system.autoUpgrade.enable = true;

  hardware.cpu.amd.updateMicrocode =
    config.hardware.enableRedistributableFirmware;

  hardware.enableRedistributableFirmware = true;

  system.stateVersion = "24.05";
}
