{ pkgs }:

{ commit, sha256 ? pkgs.lib.fakeSha256, package }:

let
  url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
  packages = import (builtins.fetchTarball {
  inherit url;
  inherit sha256;
}) {};
in
packages.${package}
