{ pkgs, ... }:
let
  fetchRev = import ../fetch-revision.nix { inherit pkgs; };
  vim = import ../local-packages/vim.nix;
  mob = import ../local-packages/mob.nix;
  gen-env = import ../local-packages/gen-env.nix;
  drag-share = import ../local-packages/drag-share.nix;
  new-meet = import ../local-packages/new-meet.nix;
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
    mu
    mpc
    solaar
    jq
    nautilus
    polkit_gnome
    qemu_kvm
    qemu
    virt-manager
    ripgrep
    xclip
    tailscale
    remmina
    direnv
    spice-gtk
    fzf
    #signal-desktop
    fd
    docker-compose
    peek
    mob
    gen-env
    drag-share
    iwgtk
    gh
    merge-github-notifs
    llm
    getmail6
    age
    neovim
    nerd-fonts.symbols-only
    notmuch
    openssl
    rofimoji
    virt-viewer
    kubectl
    wezterm
    ncmpcpp
    plexamp
    difftastic
    new-meet
    zed-editor
    delve
    opencode
    ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: [
      epkgs.treesit-grammars.with-all-grammars
      epkgs.vterm
    ]))
  ];
}
