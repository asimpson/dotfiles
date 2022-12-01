with (import <nixpkgs> {});

stdenv.mkDerivation rec {
  name = "drag-share";
  script = builtins.readFile ../../linux/drag-share.sh;
  nativeBuildInputs = [
    makeWrapper
  ];
  src = (pkgs.writeScriptBin name script).overrideAttrs(old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
  installPhase = "
    mkdir -p $out/bin
    install $src/bin/${name} $out/bin/
  ";
  fixupPhase = "wrapProgram $out/bin/${name} --prefix PATH : ${lib.makeBinPath [vlc slop]}";
}
