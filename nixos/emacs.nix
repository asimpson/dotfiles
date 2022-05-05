with (import <nixpkgs> {});

(emacs.override {
  nativeComp = true;
}).overrideAttrs (old : {
  pname = "emacs";
  version = "28.1";
  src = fetchFromSavannah {
    repo = "emacs";
    rev = "9b3481dd61735b3155a200e0c4d49f0092f52e03";
    sha256 = "01mfwl6lh79f9icrfw07dns3g0nqwc06k6fm3gr45iv1bjgg0z8g";
  };
  patches = [];
  configureFlags = old.configureFlags ++ [
    "--with-pgtk"
    "--with-cairo"
    "--with-native-compilation"
    "--with-imagemagick"
    "--with-json"
  ];
  preConfigure = "./autogen.sh";
  buildInputs = old.buildInputs ++ [
    autoconf
    texinfo
    gnutls
    pkgconfig
    gnome.gtk3
    ncurses
    xorg.libXpm
    libungif
    imagemagick
    zlib
    libgccjit
    jansson
  ];
})
