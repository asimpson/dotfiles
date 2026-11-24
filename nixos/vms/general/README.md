# ampvm

General-purpose NixOS VM: zsh, docker, tailscale (with Tailscale SSH), and an Amp runner (`--runner-id ampvm`).

## Proxmox

On the machine with these dotfiles:

```sh
cd ~/.dotfiles/nixos/vms/general
make disk
scp result/*.qcow2 root@pve:/root/ampvm.qcow2
```

On the Proxmox host (change the VMID `200`, storage `local-lvm`, and bridge `vmbr0` to fit):

```sh
qm create 200 --name ampvm --ostype l26 --machine q35 \
  --bios ovmf --efidisk0 local-lvm:1,efitype=4m,pre-enrolled-keys=0 \
  --cpu host --cores 4 --memory 8192 --agent enabled=1 \
  --net0 virtio,bridge=vmbr0 --serial0 socket \
  --scsihw virtio-scsi-single \
  --scsi0 local-lvm:0,import-from=/root/ampvm.qcow2 \
  --boot order=scsi0
qm disk resize 200 scsi0 200G
qm start 200
rm /root/ampvm.qcow2
```

- `--cpu host` is required: amp (Bun) crashes on CPU models without SSE4.2.
- `pre-enrolled-keys=0` keeps Secure Boot off; systemd-boot here is unsigned.
- The root partition grows to fill the 200G disk on first boot.

There is no password or openssh, so join the tailnet through the guest agent. This prints a login URL to open:

```sh
qm guest exec 200 -- /run/current-system/sw/bin/tailscale up --ssh --timeout 10s
```

Then, from any device on the tailnet:

```sh
ssh agent@ampvm
amp login          # the runner service starts once logged in
```

## Updating

Change `configuration.nix`, then push it to the running VM over Tailscale SSH:

```sh
make switch HOST=ampvm
```
