with (import <nixpkgs> {});

stdenv.mkDerivation rec {
  name = "new-meet";
  script = builtins.readFile ../../linux/new-meet.sh;
  src = pkgs.writeScriptBin name script;
  installPhase = "
    mkdir -p $out/bin
    install $src/bin/${name} $out/bin/
  ";
}
