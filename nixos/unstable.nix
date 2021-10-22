{ pkgs, ...}:

let u = import <unstable> { config = { allowUnfree = true; }; };

in {
  environment.systemPackages = with pkgs; [
   # u._1password-gui
    #u.discord
  ];
}
