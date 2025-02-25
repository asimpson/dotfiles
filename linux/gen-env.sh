#!/bin/sh

echo 'eval "$(lorri direnv)"' >> .envrc
echo 'use nix' >> .envrc
echo 'mkdir -p $TMPDIR' >> .envrc # https://github.com/direnv/direnv/issues/1345

cat > shell.nix<< NIX
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.go
    pkgs.gopls
  ];

  shellHook = ''
    export GOPATH=$PWD/.go
    export PATH=$PWD/.go/bin:$PATH
  '';
}
NIX
