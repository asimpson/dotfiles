let master = import (fetchTarball {
      url = "https://github.com/NixOs/nixpkgs/tarball/master";
    }) { config = { allowUnfree = true; }; };

in {
  environment.systemPackages = with master; [
    discord
    _1password-gui
    zoom-us
  ];
}
