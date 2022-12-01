#!/bin/sh

echo 'eval "$(lorri direnv)"' >> .envrc
echo 'use nix' >> .envrc

cat > shell.nix<< NIX
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [];

  shellHook = ''
  '';
}
NIX
