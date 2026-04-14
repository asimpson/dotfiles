export NIXOS_CONFIG=/home/adam/.dotfiles/nixos/vms/llm-sandbox/configuration.nix
QCOW2 := $(abspath $(wildcard result/*.qcow2))

disk:
	nixos-rebuild build-image --image-variant qemu-efi

install:
	qemu-img create -f qcow2 -F qcow2 -b "$(QCOW2)" /home/adam/llm-sandbox-overlay.qcow2

.PHONY: install disk
