with (import <nixpkgs> {});

(vim.overrideAttrs (old : {
  pname = "vim";
  buildInputs = old.buildInputs ++ [
    xorg.libXt
  ];
}))
