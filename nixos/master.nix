let master = import (fetchTarball {
      url = "https://github.com/NixOs/nixpkgs/tarball/master";
    }) { config = { allowUnfree = true; }; };

in {

  programs = with master; {
    _1password-gui = {
      enable = true;
      #gid = 4000;
      polkitPolicyOwners = [ "adam" ];
    };
  };

  environment.systemPackages = with master; [
    # discord
    _1password-gui
    zoom-us
  ];
}
