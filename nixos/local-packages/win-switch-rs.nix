{ pkgs }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "win-switch-rs";
  version = "0.1.1";

  src = pkgs.fetchurl {
    url = "https://git.sr.ht/~asimpson/win-switch-rs/archive/b6e243ebeae0f8856b206587dc64ddf9307c410e.tar.gz";
    hash = "sha256-SbLkm7e+w3fqU+zcjmMV8eAUQuUuwzESRiCN6nogPeU=";
  };

  cargoHash = "sha256-TJ+v8AiI+8zai3a0exNywDZt3LozfRE/nV77Tr7IgIo=";

  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.systemd ];

  meta = with pkgs.lib; {
    description = "Switch monitor inputs/brightness via DDC/CI";
    homepage = "https://git.sr.ht/~asimpson/win-switch-rs";
    mainProgram = "win-switch-rs";
    platforms = platforms.linux;
  };
}
