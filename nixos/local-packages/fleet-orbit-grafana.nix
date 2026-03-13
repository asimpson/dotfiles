{ pkgs, src ? null }:

let
  version = "1.50.2-1";
  packageName = "fleet-osquery-${version}-x86_64.pkg.tar.zst";
  sourceHash = "0m6ah874x3nmk7i7ddamxa5408d85da8askr5is19ghcrpk6gcah";
  packageSrc = if src != null then src else pkgs.requireFile {
    name = packageName;
    sha256 = sourceHash;
    url = "https://drive.google.com/drive/folders/1OMAHNk2cON4iTH_CSIQcuoH6s-_xYm6_";
    message = ''
      Download ${packageName} from the Grafana Fleet Google Drive and add it
      to your local Nix store:

        nix-store --add-fixed sha256 /path/to/${packageName}
    '';
  };

in
pkgs.stdenvNoCC.mkDerivation {
  pname = "fleet-orbit-grafana";
  inherit version;
  src = packageSrc;
  passthru = {
    source = packageSrc;
    inherit packageName sourceHash;
  };

  nativeBuildInputs = [ pkgs.zstd pkgs.gnutar ];

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;
  dontFixup = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$TMPDIR/unpack"
    zstd --decompress --stdout "$src" | tar -xf - -C "$TMPDIR/unpack"

    install -Dm755 \
      "$TMPDIR/unpack/opt/orbit/bin/orbit/linux/stable/orbit" \
      "$out/bin/orbit"

    runHook postInstall
  '';
}
