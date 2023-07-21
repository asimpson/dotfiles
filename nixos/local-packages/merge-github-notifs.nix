with (import <nixpkgs> {});

stdenv.mkDerivation rec {
  name = "merge-github-notifs";
  script = builtins.readFile ../../linux/merge-github-notifs.sh;
  src = (pkgs.writeScriptBin name script).overrideAttrs(old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
  installPhase = "
    mkdir -p $out/bin
    install $src/bin/${name} $out/bin/
  ";
}
