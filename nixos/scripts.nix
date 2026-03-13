{ pkgs, ... }:
let
  mail-preview = pkgs.writeScriptBin "mail-preview" (builtins.readFile ../linux/mail-preview);
  toggle-dunst = pkgs.writeScriptBin "toggle-dunst" (builtins.readFile ../linux/toggle-dunst.sh);
  mpv-yt = pkgs.writeScriptBin "mpv-yt" (builtins.readFile ../linux/mpv-yt.sh);
  change-audio = pkgs.writeScriptBin "change-audio" (builtins.readFile ../linux/rofi-change-audio.sh);
  kolide-vm = pkgs.writeScriptBin "kolide-vm" (builtins.readFile ../linux/kolide.sh);
  provision-nix = pkgs.writeScriptBin "provision-nix" (builtins.readFile ../linux/provision-nix.sh);
in
{
  environment.systemPackages = [
    mail-preview
    toggle-dunst
    mpv-yt
    change-audio
    kolide-vm
    provision-nix
  ];
}
