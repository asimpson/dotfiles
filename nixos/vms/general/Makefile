export NIXOS_CONFIG=/home/adam/.dotfiles/nixos/vms/general/configuration.nix

disk:
	nixos-rebuild build-image --image-variant qemu-efi

#BACKING should be the full /nix path, not just /result.
install:
	qemu-img create -f qcow2 -F qcow2 -b "$(BACKING)" /home/adam/ampvm-overlay.qcow2 200G

reset:
	virsh destroy ampvm

# update a running vm in place: make switch HOST=<ip>
switch:
	nixos-rebuild switch --target-host agent@$(HOST) --sudo

.PHONY: install disk reset switch
