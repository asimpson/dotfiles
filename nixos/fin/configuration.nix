# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  xmodmap = pkgs.writeText ".Xmodmap" ''
    keycode 66 = Multi_key
    clear Lock
  '';
in {
  require = [ ../scripts.nix ./packages.nix ./crate.nix ../master.nix ];

  imports = [
    ./hardware-configuration.nix
    (fetchTarball
      "https://github.com/nix-community/nixos-vscode-server/tarball/master")
  ];

  fileSystems."/home/adam/mail" = {
    device = "shire/root/home/mail";
    fsType = "zfs";
  };

  fileSystems."/home/adam/Source" = {
    device = "shire/source";
    fsType = "zfs";
  };

  fileSystems."/home/adam/Projects" = {
    device = "shire/projects";
    fsType = "zfs";
  };

  boot = rec {
    kernelPackages = pkgs.linuxPackages_6_1;
    extraModulePackages = [ kernelPackages.v4l2loopback ];
    zfs.requestEncryptionCredentials = true;

    #is required if you are running an newer kernel which is not yet officially supported by zfs
    #otherwise the zfs module will refuse to evaluate and show up as broken
    #zfs.enableUnstable = true;

    kernelParams = [ "intel_iommu=on" "i915.force_probe=4680" ]; #quiet doesn't work
    kernelModules = [
      "v4l2loopback"
      "kvm-intel"
      "vfio_virqfd"
      "vfio_pci"
      "vfio_iommu_type1"
      "vfio"
    ];
    extraModprobeConfig = "options vfio-pci ids=10de:2487,10de:228b";

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.copyKernels = true;
    };

    supportedFilesystems = [ "zfs" ];
    kernel.sysctl = {
      "dev.i915.perf_stream_paranoid" = 0;
      "net.ipv4.ip_forward" = 1;
      "net.ipv6.conf.all.forwarding" = 1;
    };

    #consoleLogLevel = 0;
    #initrd.verbose = false;
  };

  nixpkgs.config.allowUnfree = true;

  systemd.services = {
    backupmail = {
      path = [
        pkgs.age
      ];
      script = ''
        set -eu
        ${pkgs.getmail6}/bin/getmail
      '';
      serviceConfig = {
        User = "adam";
      };
      startAt = "hourly";
    };
  };

  nix = {
    settings = {
      auto-optimise-store = true;
      allowed-users = [ "@wheel" ];
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  time.timeZone = "America/New_York";

  networking = {
    interfaces = { enp8s0 = { wakeOnLan.enable = true; }; };
    firewall = {
      enable = true;
      allowedTCPPorts = [ 3000 8080 ];
      allowedUDPPorts = [ 41641 ];
      checkReversePath = "loose";
    };
    hostId = "47ffe1b9"; # head -c4 /dev/urandom | od -A none -t x4
    hostName = "fin";
    iproute2.enable = true;
    wireless = { iwd = { enable = true; }; };
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

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  hardware.logitech.wireless.enable = true;
  hardware.i2c.enable = true;

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  security.polkit.enable = true;
  programs.dconf.enable = true;
  programs.zsh.enable = true;
  programs.ssh.startAgent = true;
  programs.light.enable = true;
  programs.gnupg.agent.enable = true;

  users.users.adam = {
    isNormalUser = true;
    extraGroups = [ "docker" "wheel" "lp" "video" "audio" "libvirtd" ];
    shell = pkgs.zsh;
  };

  services = {
    libinput.enable = true;
    displayManager = {
      defaultSession = "none+i3";
    };
    vscode-server.enable = true;
    syncthing = {
      enable = true;
      user = "adam";
      dataDir = "/home/adam/Sync";
      configDir = "/home/adam/.config/syncthing";
      settings = {
        devices = {
          astromech = {
            id =
              "ITT5GJ7-2YMQNSL-6L5WYRA-FC3YPAO-ON6WKRT-HO27JBK-WVZGPCT-UTCEJQO";
          };
        };
        folders = {
          "/home/adam/Shared" = {
            id = "x2lgj-4mf6q";
            devices = [ "astromech" ];
            label = "Shared";
          };
        };
      };
    };

    xserver = {
      xkb = {
        layout = "us";
      };
      enable = true;

      desktopManager = { xterm.enable = false; };

      displayManager = {
        sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${xmodmap}";
      };

      windowManager.i3 = {
        enable = true;
        extraPackages = with pkgs; [
          rofi # application launcher most people use
          i3status # gives you the default i3 status bar
        ];
      };
    };
    lorri.enable = true;
    mpdscribble = {
      enable = true;
      endpoints = {
        "last.fm" = {
          username = "skimpson";
          passwordFile = "/home/adam/lastfm-pass";
        };
      };
    };
    tailscale.enable = true;
    resolved.enable = true;
    gnome.sushi.enable = true;
    printing = {
      enable = true;
      drivers = [ pkgs.brlaser ];
    };
    #avahi.enable = true;
    openssh.enable = true;
    gnome.gnome-keyring.enable = true;

    zfs = {
      autoSnapshot.enable = true;
      autoScrub.enable = true;
      autoSnapshot.monthly = 3;
    };

    timesyncd = {
      enable = true;
      servers = [ "time.google.com" ];
    };

    acpid.enable = true;
    colord.enable = true;
    fwupd.enable = true;
    cron.enable = true;
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    udev = {
      extraRules = ''
        LABEL="gmk pro regular user access"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="320f", ATTRS{idProduct}=="5044", TAG+="uaccess"
      '';
    };
  };

  virtualisation = {
    spiceUSBRedirection.enable = true;
    libvirtd = {
      enable = true;

      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;
        ovmf = {
          enable = true;
          packages = [ pkgs.OVMFFull.fd ];
        };
      };
    };

    docker = {
      enable = true;
      autoPrune.enable = true;
      storageDriver = "overlay2";
    };
  };

  security.pam = {
    u2f.enable = true;
    u2f.cue = true;
    u2f.authFile = "/home/adam/.config/Yubico/u2f_keys";

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

  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-unstable";

  environment.etc."polkit-gnome-authentication-agent-1".source =
    "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";

  #environment.etc."X11/xorg.conf.d/20-intel.conf" = {};
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
