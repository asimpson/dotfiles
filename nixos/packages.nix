{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    vim
    firefox
    ffmpeg-full
    mpv
    slack
    rofi
    google-chrome
    #mpd
    tilix
    brightnessctl
    acpid
    desktop-file-utils
    signal-desktop
    flameshot
    gnome.networkmanagerapplet
    gnome.networkmanager-vpnc
    emacsPgtk
    git
    hack-font
    docker
    bluez
    bluez-tools
    pavucontrol
    hunspell
    hunspellDicts.en-us
    obs-studio
    clipster
    dunst
    isync
    calibre
    mu
    mpc_cli
    solaar
    jq
    gnome3.nautilus
    polkit_gnome
    rofi-emoji
    rofi-mpd
    qemu_kvm
    qemu
    virt-manager
    ripgrep
    xclip
    tailscale
    iwd
    nasc
    xsettingsd
    remmina
    direnv
    spice-gtk
  ];
}
