# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  require = [
    ./crate.nix
    ../scripts.nix
    ../packages.nix
  ];

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;
  boot = rec {
    kernelPackages = pkgs.linuxPackages_5_11;
    kernelModules = [ "mba6x_bl" ];
    extraModulePackages = [kernelPackages.acpi_call kernelPackages.mba6x_bl kernelPackages.v4l2loopback];
    zfs.requestEncryptionCredentials = true;
    kernelParams = ["acpi_backlight=native" "hid_apple.iso_layout=0" "acpi_osi=" "hid_apple.swap_opt_cmd=1"];

    loader = {
      # Use the systemd-boot EFI boot loader.
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.copyKernels = true;
    };
    kernel.sysctl = {
      "vm.swappiness" = lib.mkDefault 1;
    };

    supportedFilesystems = ["zfs"];
  };

  hardware.bluetooth.enable = true;
  hardware.facetimehd.enable = lib.mkDefault
    (config.nixpkgs.config.allowUnfree or false);

  services.fstrim.enable = lib.mkDefault true;

  networking.hostName = "mba-nixos"; # Define your hostname.
  networking.hostId = "4a7f83ce"; # head -c4 /dev/urandom | od -A none -t x4
  #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp61s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };
  fonts = {
    enableDefaultFonts = true;
    # Give fonts to 32-bit binaries too (e.g. steam).
    fontconfig.cache32Bit = true;
    fonts = with pkgs; [
        google-fonts liberation_ttf opensans-ttf roboto roboto-mono
    ];
  };


  # Enable the GNOME 3 Desktop Environment.
  #services.xserver.displayManager.gdm.enable = true;
  #services.xserver.desktopManager.gnome3.enable = true;

  # Configure keymap in X11
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  hardware.cpu.intel.updateMicrocode = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.adam = {
    isNormalUser = true;
    extraGroups = [ "docker" "wheel" "lp" "video" "audio"]; # Enable ‘sudo’ for the user.
  };

  programs.dconf.enable = true;
  programs.ssh.startAgent = true;
  programs.ssh.forwardX11 = true;
  hardware.enableRedistributableFirmware = true;
  hardware.trackpoint.enable = true;
  hardware.opengl = {
    enable = true;
    #extraPackages = with pkgs; [
    #  intel-
    #];
  };
  documentation.dev.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
  #   enableSSHSupport = true;
  };

  # List services that you want to enable:

  services = {
    openssh.enable = true;
    blueman.enable = true;
    gnome.gnome-keyring.enable = true;

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
          i3lock #default i3 screen locker
          #i3blocks #if you are planning on using i3blocks over i3status
       ];
      };
      deviceSection = lib.mkDefault ''
        Option "Backlight" "mba6x_backlight"
        Option "TearFree" "true"
      '';
    };

    tlp.enable = true;
    acpid.enable = true;
    colord.enable = true;
    fprintd.enable = true;
    fwupd.enable = true;
    geoip-updater.enable = true;
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };

  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
  };

  security.pam = {
    u2f.enable = true;
    u2f.cue = true;

    services = {
      login.u2fAuth = true;
      lightdm.u2fAuth = true;
      sudo.u2fAuth = true;
      lightdm.enableGnomeKeyring = true;
    };
  };

  services.mpd.extraConfig = ''
    audio_output {
      type "alsa"
      name "alsa-pipe"
    }
  '';

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  networking.wg-quick.interfaces = {
    wg0 = {
      address = [ "172.16.14.9/32" ];
      dns = [ "172.16.12.1" ];
      privateKeyFile = "/home/adam/privatekey";

      peers = [
        {
          publicKey = "of6hIl1rgajTt4PD1QwYMiE4+jgqmtar+D8XY8lLtks=";
          allowedIPs = [ "0.0.0.0/0" "::/0" ];
          endpoint = "home.simpsonfam.com:45340";
        }
      ];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
