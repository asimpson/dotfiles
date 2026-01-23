#!/usr/bin/env bash

set -e

# Ask questions
read -p "Do you need Go? (y/n): " need_go
read -p "Do you need Node? (y/n): " need_node

need_go=$(echo "$need_go" | tr '[:upper:]' '[:lower:]')
need_node=$(echo "$need_node" | tr '[:upper:]' '[:lower:]')

# Validate at least one is selected
if [[ "$need_go" != "y" && "$need_node" != "y" ]]; then
    echo "You must select at least one of Go or Node."
    exit 1
fi

# Build the shell.nix content
build_inputs=""
shell_hook_dirs=""
shell_hook_exports=""

if [[ "$need_go" == "y" ]]; then
    build_inputs+="    pkgs.go
    pkgs.gopls"
    shell_hook_dirs+=' $DIRECTORY_NAME/go-cache $DIRECTORY_NAME/go'
    shell_hook_exports+='    export GOPATH=$DIRECTORY_NAME/go
    export GOCACHE=$DIRECTORY_NAME/go-cache
    export PATH=$GOPATH/bin:$PATH'
fi

if [[ "$need_node" == "y" ]]; then
    if [[ -n "$build_inputs" ]]; then
        build_inputs+=$'\n'
    fi
    build_inputs+="    (pkgs.yarn.override { nodejs = pkgs.nodejs_22; })
    pkgs.nodejs_22"
    shell_hook_dirs+=' $DIRECTORY_NAME/npm'

    node_export='    export NODE_PATH=$DIRECTORY_NAME/npm
    export NPM_CONFIG_PREFIX=$NODE_PATH'

    if [[ "$need_go" == "y" ]]; then
        # Insert NODE_PATH/bin before $PATH
        shell_hook_exports=$(echo "$shell_hook_exports" | sed 's|\$GOPATH/bin:\$PATH|\$GOPATH/bin:\$NODE_PATH/bin:\$PATH|')
        shell_hook_exports+=$'\n'"$node_export"
    else
        shell_hook_exports+="$node_export"
        shell_hook_exports+=$'\n    export PATH=$NODE_PATH/bin:$PATH'
    fi
fi

# Build hardeningDisable line (needed for Go)
hardening_disable=""
if [[ "$need_go" == "y" ]]; then
    hardening_disable=$'\n  hardeningDisable = [ "fortify" ];'
fi

# Generate shell.nix
cat > shell.nix << EOF
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
$build_inputs
  ];$hardening_disable

  shellHook = ''
    DIRECTORY_NAME="/tmp/\$(basename "\$PWD")"
    mkdir -p$shell_hook_dirs
$shell_hook_exports
  '';
}
EOF

echo "Created shell.nix"

# Check and create .envrc if needed
if [[ ! -f .envrc ]]; then
    echo "use nix" > .envrc
    echo "Created .envrc with 'use nix'"
else
    echo ".envrc already exists, skipping"
fi

echo "Done! Run 'direnv allow' to activate the environment."
