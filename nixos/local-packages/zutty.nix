with (import <nixpkgs> {});

stdenv.mkDerivation rec {
  pname = "zutty";
  version = "0.11";

  src = fetchFromGitHub {
    owner = "tomszilagyi";
    repo = pname;
    rev = version;
    hash = "sha256-OUBGNLasFo1EmlpDE31CVu7u7B+cWcCB7ASqfU56i4k=";
  };

  nativeBuildInputs = [
    wafHook
    python3
    pkg-config
  ];

  buildInputs = [
    freetype
    libglvnd
    xorg.libXmu
  ];

  postPatch = ''
    substituteInPlace src/options.h \
      --replace '/usr/share/fonts' '${xorg.fontmiscmisc}/lib/X11/fonts/misc'
  '';

  postInstall = ''
    install -Dm644 -t $out/share/doc/${pname} doc/*
  '';
}
