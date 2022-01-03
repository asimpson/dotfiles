# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  require = [
    ./crate.nix
    ../scripts.nix
    ../packages.nix
    ../unstable.nix
    ../local.nix
  ];

  imports =
    [ # Include the results of the hardware scan.
      <nixos-hardware/lenovo/thinkpad/t480s>
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  nix = {
    autoOptimiseStore = true;
    allowedUsers = [ "@wheel" ];
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  # Use the systemd-boot EFI boot loader.

  boot = rec {
    kernelPackages = pkgs.linuxPackages_5_10;
    extraModulePackages = [kernelPackages.v4l2loopback];
    zfs.requestEncryptionCredentials = true;
    kernelParams = ["acpi_backlight=native" "elevator=none"]; #https://grahamc.com/blog/nixos-on-zfs
    kernelModules = ["v4l2loopback" "kvm-intel"];

    loader = {
      # Use the systemd-boot EFI boot loader.
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.copyKernels = true;
    };

    supportedFilesystems = ["zfs"];
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  networking = {
    hostName = "simpson-nixos"; # Define your hostname.
    firewall.enable = true;
    wireless.iwd.enable = true;
    hostId = "ed6bb572"; # head -c4 /dev/urandom | od -A none -t x4
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;

    interfaces.enp0s31f6 = {
      useDHCP = true;
      wakeOnLan =  {
        enable = true;
      };
    };

    extraHosts =
    ''
      127.0.0.1 sparkbox.local
    '';

    networkmanager = {
      enable = true;
      wifi.powersave = true;
      wifi.backend = "iwd";
    };

    iproute2.enable = true;
    useHostResolvConf = false;
  };

  fonts = {
    enableDefaultFonts = true;
    # Give fonts to 32-bit binaries too (e.g. steam).
    fontconfig.cache32Bit = true;
    fonts = with pkgs; [
        hack-font google-fonts liberation_ttf opensans-ttf roboto roboto-mono
    ];
  };

  sound.enable = true;
  # Enable touchpad support (enabled default in most desktopManager).
  #services.xserver.libinput.enable = true;

  users.users.adam = {
    isNormalUser = true;
    extraGroups = [ "bluetooth" "docker" "wheel" "lp" "video" "audio" "libvirtd" ];
    shell = pkgs.zsh;
  };

  security.polkit.enable = true;
  programs.dconf.enable = true;
  programs.zsh.enable = true;
  programs.ssh.startAgent = true;
  programs.light.enable = true;

  hardware = {
    enableRedistributableFirmware = true;
    trackpoint.enable = true;
    opengl = {
      enable = true;
      #extraPackages = with pkgs; [
      #  intel-
      #];
    };
    logitech.wireless.enable = true;
    # install via nix-env for pactl
    pulseaudio.enable = false;
    bluetooth.enable = true;
    i2c.enable = true;
    cpu.intel.updateMicrocode = true;
  };

  documentation.dev.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
  #   enableSSHSupport = true;
  };

  programs.sway = {
    enable = false;
    wrapperFeatures.gtk = true; # so that gtk works properly
    extraPackages = with pkgs; [
      swaylock
      swayidle
      wl-clipboard
      mako # notification daemon
      alacritty # Alacritty is the default terminal in the config
      dmenu # Dmenu is the default in the config but i recommend wofi since its wayland native
      xwayland
    ];
    extraSessionCommands = ''
      export MOZ_ENABLE_WAYLAND=1
      export XDG_CURRENT_DESKTOP=sway
    '';
  };

  #environment.loginShellInit = ''
  #  if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
  #    exec sway
  #  fi
  #'';

  # List services that you want to enable:

  services = {
    lorri = {
      enable = true;
    };
    syncthing = {
      enable = true;
      user = "adam";
      dataDir = "/home/adam/Sync";
      configDir = "/home/adam/.config/syncthing";
      declarative = {
        devices = {
          wemm = {
            id = "A7BKRRY-5AIRT4B-YP4DXYX-RL7SKZ5-52TKEY2-NNJGBZ6-YF423ON-TCW7MQM";
          };
          ged = {
            id = "GNI7GNS-BYK6EPE-4J7PJNF-D3EQ7DS-5OHVKF2-3RIB3UD-V6AYEK7-XLVKFQ5";
          };
        };
        folders = {
          "/home/adam/notes" = {
            id = "qjwus-uru7t";
            devices = [ "wemm" "ged" ];
            label = "notes";
          };
        };
      };
    };
    resolved = {
      enable = true;
      dnssec = "false";
    };
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
    gnome.sushi.enable = true;
    printing.enable = true;
    openssh.enable = true;
    blueman.enable = true;
    gnome.gnome-keyring.enable = true;

    zfs = {
      autoSnapshot.enable = true;
      autoScrub.enable = true;
    };

    timesyncd = {
      enable = true;
      servers = ["time.google.com"];
    };

    xserver = {
      layout = "us";
      enable = true;
      libinput.enable = true;

      desktopManager = {
        xterm.enable = false;
      };

      displayManager = {
        defaultSession = "none+i3";
      };

      windowManager.i3 = {
        enable = true;
        extraPackages = with pkgs; [
          rofi #application launcher most people use
          i3status # gives you the default i3 status bar
       ];
      };
    };

    tlp.enable = true;
    acpid.enable = true;
    colord.enable = true;
    fprintd.enable = true;
    fwupd.enable = true;
    cron.enable = true;
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      config.pipewire = {
        "api.alsa.ignore-dB" = true;
      };
    };

    udev = {
      extraRules = ''
        ACTION!="add|change", GOTO="end_disable_infared"
        ATTRS{idVendor}=="04f2", ATTRS{idProduct}=="b615", ATTR{bConfigurationValue}="0"
        LABEL="end_disable_infared"
        LABEL="gmk pro regular user access"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", TAG+="uaccess"
        SUBSYSTEM=="drm", ACTION=="change", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/adam/.Xauthority", RUN+="${pkgs.bash}/bin/bash /home/adam/.dotfiles/linux/i3-monitor-change.sh"
      '';
    };
  };

  virtualisation.libvirtd.enable = true;
  virtualisation.libvirtd.qemuRunAsRoot = true;
  virtualisation.libvirtd.qemuPackage = pkgs.qemu_kvm;

  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
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

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  #polkit-gnome doesn't show up in the normal /usr location so I symlink it out of the store to /etc/polkit-gnome-authentication-agent-1
  environment.etc."polkit-gnome-authentication-agent-1".source = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";

  system.autoUpgrade.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
