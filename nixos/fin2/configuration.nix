{ config, lib, pkgs, ... }:

let
  rofi-clipster = pkgs.stdenv.mkDerivation rec {
    pname = "rofi-clipster";
    version = "unstable-2023-10-01";
    
    src = pkgs.fetchFromGitHub {
      owner = "fdw";
      repo = "rofi-clipster";
      rev = "main";
      sha256 = "sha256-tOi5cBFs/oWwRm58iFje1wndjzXD85IM7zoLOJ6IUQE=";#lib.fakeSha256;
    };
    
    buildInputs = [ pkgs.python3 ];
    
    installPhase = ''
      mkdir -p $out/bin
      cp src/clippy/clippy.py $out/bin/rofi-clipster
      chmod +x $out/bin/rofi-clipster
      
      # Fix the shebang
      substituteInPlace $out/bin/rofi-clipster \
        --replace "#!/usr/bin/env python3" "#!${pkgs.python3}/bin/python3" \
        --replace "#!/usr/bin/python3" "#!${pkgs.python3}/bin/python3"
      
      # Make sure rofi is in PATH when the script runs
      wrapProgram $out/bin/rofi-clipster \
        --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.rofi pkgs.clipster ]}
    '';
    
    nativeBuildInputs = [ pkgs.makeWrapper ];
    
    meta = with pkgs.lib; {
      description = "A rofi frontend for clipster";
      homepage = "https://github.com/fdw/rofi-clipster";
      maintainers = [ ];
    };
  };

in

{
  require = [ ../fin/packages.nix ];
  imports = [ ./hardware-configuration.nix ];

  environment.systemPackages = with pkgs; [
    _1password-gui
    zoom-us
    vscode
    nixfmt-rfc-style
    rofi-clipster
    feh
    pulseaudio
    bat
  ];

  system.activationScripts.binbash = {
    text = ''
      if [ ! -f "/bin/bash" ]; then
        ln -sf ${pkgs.bash}/bin/bash /bin/bash
      fi
    '';
    deps = [ ];
  };

  boot = {
    # ZFS support
    zfs.requestEncryptionCredentials = true;
    supportedFilesystems = [ "zfs" ];

    # Bootloader
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.copyKernels = true;
      grub.configurationLimit = 20;
    };

    # v4l2loopback for virtual camera
    extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];

    # Kernel modules (updated for AMD)
    kernelModules = [ "v4l2loopback" "kvm-amd" ];

    # System tuning
    kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
      "net.ipv6.conf.all.forwarding" = 1;
    };
  };

  nixpkgs.config.allowUnfree = true;
  time.timeZone = "America/New_York";
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

  networking = {
    #extraHosts = hosts.hosts;
    interfaces = { enp9s0 = { wakeOnLan.enable = true; }; };
    firewall = {
      enable = true;
      allowedTCPPorts = [ 3000 8080 ];
      allowedUDPPorts = [ 41641 ];
      checkReversePath = "loose";
      #extraCommands = hosts.extra;
    };
    hostName = "fin2";
    hostId = "5a488f19";
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
        ovmf = {
          enable = true;
          packages = [ pkgs.OVMFFull.fd ];
        };
      };
    };
    docker = {
      enable = true;
      autoPrune.enable = true;
      storageDriver = "zfs";
      extraOptions = "--data-root=/var/lib/docker";
    };
  };

  # Set up the enrollment secret
  environment.etc."kolide-k2/secret" = {
    mode = "0600";
    text =
      "";
  };
  environment.etc."kolide-k2/launcher.flags" = {
    mode = "0744";
    text = ''
      autoupdate
      update_channel stable
      transport jsonrpc
      hostname k2device.kolide.com
      root_directory /opt/kolide-k2/k2device.kolide.com
      osqueryd_path /opt/kolide-k2/bin/osqueryd
      enroll_secret_path /etc/kolide-k2/secret
    '';
  };

  systemd.services.kolide-launcher = {
    description = "Kolide Launcher";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "simple";
      ExecStart =
        "${pkgs.steam-run}/bin/steam-run /opt/kolide-k2/bin/launcher --config=/opt/kolide-k2/etc/launcher.flags"; # Use full path
      Restart = "always";
      User = "root";
      RemainAfterExit = false;
      KillMode = "mixed";
      KillSignal = "SIGTERM";
      TimeoutStopSec = "30s";
    };

    wants = [ "network-online.target" ];
  };
  programs.steam.enable = true;

  services = {
    xserver = {
      xkb = { layout = "us"; };
      enable = true;

      desktopManager = { xterm.enable = false; };

      displayManager = {
        #sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${xmodmap}";
      };

      windowManager.i3 = {
        enable = true;
        extraPackages = with pkgs; [
          rofi # application launcher most people use
          i3status # gives you the default i3 status bar
        ];
      };
    };
    zfs = {
      autoScrub.enable = true;
      autoScrub.interval = "weekly";
      autoSnapshot.enable = true;
      trim.enable = true;
      trim.interval = "weekly";
    };
    displayManager = { defaultSession = "none+i3"; };
    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "yes"; # Change to "no" after setup
        PasswordAuthentication = true; # Change to false if using keys
      };
    };
    tailscale.enable = true;
    resolved.enable = true;
    gnome.sushi.enable = true;
    printing = {
      enable = true;
      drivers = [ pkgs.brlaser ];
    };
    gnome.gnome-keyring.enable = true;
  };

  # User configuration
  users.users.adam = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "audio" "video" "libvirtd" "lp" "wheel" "docker" ];
    # Uncomment and set password hash or add SSH keys here
    # hashedPassword = "...";
    # openssh.authorizedKeys.keys = [ "..." ];
  };

  system.autoUpgrade.enable = true;

  # AMD 9950X specific optimizations
  hardware.cpu.amd.updateMicrocode =
    config.hardware.enableRedistributableFirmware;

  # Enable firmware updates
  hardware.enableRedistributableFirmware = true;

  system.stateVersion = "24.05";
}
