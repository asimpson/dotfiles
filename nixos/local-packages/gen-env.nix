with (import <nixpkgs> {});

stdenv.mkDerivation rec {
  name = "gen-env";
  script = builtins.readFile ../../linux/gen-env.sh;
  src = (pkgs.writeScriptBin name script).overrideAttrs(old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
  installPhase = "
    mkdir -p $out/bin
    install $src/bin/${name} $out/bin/
  ";
}
