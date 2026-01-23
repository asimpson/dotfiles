{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  pname = "rofi-clipster";
  version = "unstable-2023-10-01";

  src = pkgs.fetchFromGitHub {
    owner = "fdw";
    repo = "rofi-clipster";
    rev = "main";
    sha256 = "sha256-tOi5cBFs/oWwRm58iFje1wndjzXD85IM7zoLOJ6IUQE=";#lib.fakeSha256;
  };

  buildInputs = [ pkgs.python3 ];
  nativeBuildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp src/clippy/clippy.py $out/bin/rofi-clipster
    chmod +x $out/bin/rofi-clipster

    # Fix the shebang
    substituteInPlace $out/bin/rofi-clipster \
      --replace "#!/usr/bin/env python3" "#!${pkgs.python3}/bin/python3" \
      --replace "#!/usr/bin/python3" "#!${pkgs.python3}/bin/python3"

    # Make sure rofi is in PATH when the script runs
    wrapProgram $out/bin/rofi-clipster \
      --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.rofi pkgs.clipster ]}
  '';
}