export NIXOS_CONFIG=/home/adam/.dotfiles/nixos/vms/llm-sandbox/configuration.nix

disk:
	nixos-rebuild build-image --image-variant qemu-efi

#BACKING should be the full /nix path, not just /result.
install:
	qemu-img create -f qcow2 -F qcow2 -b "$(BACKING)" /home/adam/llm-sandbox-overlay.qcow2 50G

reset:
	virsh destroy llm-jail

.PHONY: install disk
