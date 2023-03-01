{ pkgs, ...}:

let u = import (/home/adam/Source/nixpkgs) { config = { allowUnfree = true; }; };

in {
  environment.systemPackages = with pkgs; [
    u.discord
    #u._1password-gui
    u.zoom-us
    u.mpd
  ];
}
