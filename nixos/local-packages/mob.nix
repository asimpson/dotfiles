with (import <nixpkgs> {});

(mob.overrideAttrs (old : rec {
  pname = "mob";
  version = "3.1.3";

  src = fetchFromGitHub {
    owner = "remotemobprogramming";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256:12aplqlykblll9d8svlnva4x8m5yf09x2950nvq5j87pwz06prhl";
  };
}))
