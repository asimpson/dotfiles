{ pkgs ? import <nixpkgs> {} }:

pkgs.buildGoModule {
  src = pkgs.fetchFromGitHub {
    owner = "parnoldx";
    repo = "nascTUI";
    rev = "8febe758e0877aeded818f9ed2f5b9651c429566";
    hash = "sha256-QoO0tbGl0l7hE2n+X5374Y33+iuTpTKz1UZpXAfUrXQ="; #pkgs.lib.fakeHash;
  } + "/src";
  name = "nasc";
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.libqalculate ];
  vendorHash = "sha256-ys9VEv8PisvO9UCD6M3aLrJeF88ZNAUATxyTVV02z44="; #pkgs.lib.fakeHash;
}
