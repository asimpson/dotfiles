{ pkgs, ... }:
let
  myEmacs = import ../local-packages/emacs.nix;
  vim = import ../local-packages/vim.nix;
  mob = import ../local-packages/mob.nix;
  gen-env = import ../local-packages/gen-env.nix;
  drag-share = import ../local-packages/drag-share.nix;
  merge-github-notifs = import ../local-packages/merge-github-notifs.nix;

in {
  environment.systemPackages = with pkgs; [
    vim
    zip
    unzip
    bash
    firefox
    ffmpeg-full
    mpv
    slack
    rofi
    google-chrome
    mpd
    tilix
    desktop-file-utils
    flameshot
    git
    hack-font
    docker
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
    qemu_kvm
    qemu
    virt-manager
    ripgrep
    xclip
    tailscale
    #nasc
    remmina
    direnv
    spice-gtk
    fzf
    signal-desktop
    fd
    docker-compose
    peek
    mob
    gen-env
    vscode
    drag-share
    iwgtk
    gh
    merge-github-notifs
    #llm
    getmail6
    age
    neovim
    nerdfonts
    notmuch
  ];
}
