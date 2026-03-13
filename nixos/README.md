# Fleet Operations (fin2)

This is the minimum runbook for keeping Fleet Orbit working on `fin2`.

## Files that matter

- `fin2/configuration.nix`: Fleet service, compatibility, and host wiring
- `local-packages/fleet-orbit-grafana.nix`: pinned Fleet package version/hash
- `fleet-key.nix`: Fleet enroll secret

## Getting the enroll secret

Common case at Grafana: the secret is embedded in the provided
`fleet-osquery-<version>-x86_64.pkg.tar.zst` artifact.

You can inspect it from the package:

```bash
zstd -dc /path/to/fleet-osquery-<version>-x86_64.pkg.tar.zst \
  | tar -xOf - etc/default/orbit \
  | rg '^ORBIT_ENROLL_SECRET='
```

Then set `fleet-key.nix` to that value and rebuild.

If the artifact does not contain it, request the current host enrollment secret
from Fleet/IT admins (usually via secure channel or a newly published artifact).

## Daily operations

```bash
systemctl status orbit.service --no-pager -l
journalctl -u orbit.service -n 120 --no-pager
sudo systemctl restart orbit.service
```

## First-time setup (or if package is missing)

1. Download the Grafana-provided file:
   `fleet-osquery-<version>-x86_64.pkg.tar.zst`
2. Add it to the Nix store:

```bash
nix-store --add-fixed sha256 /path/to/fleet-osquery-<version>-x86_64.pkg.tar.zst
```

3. Apply config:

```bash
sudo nixos-rebuild switch
```

## Update Fleet bootstrap version

1. Download new `fleet-osquery-<new-version>-x86_64.pkg.tar.zst`
2. Get hash (must be `--flat`):

```bash
nix-hash --type sha256 --base32 --flat /path/to/fleet-osquery-<new-version>-x86_64.pkg.tar.zst
```

3. Update in `local-packages/fleet-orbit-grafana.nix`:
   - `version = "<new-version>";`
   - `sourceHash = "<new-base32-hash>";`
4. Add the new file to store:

```bash
nix-store --add-fixed sha256 /path/to/fleet-osquery-<new-version>-x86_64.pkg.tar.zst
```

5. Rebuild:

```bash
sudo nixos-rebuild switch
```

The first rebuild after adding a new Fleet artifact is important: this config
pins the `.zst` into the system closure so `nixos-upgrade.service` will still
find it later, even after garbage collection.

## Enrollment and SSO

If logs show:
- `enroll request: end user authentication required`

then open the SSO URL from logs and complete login, or ask IT to assign your user in Okta/Fleet.

Quick URL extraction:

```bash
journalctl -u orbit.service -n 200 --no-pager | rg -o 'https://[^ ]+/mdm/sso[^ ]*' | tail -1
```

## Known-good signals

Fleet side is healthy when logs show:
- `orbit version: 1.52.1` (or current)
- `Found osquery version: ...`
- no repeated `exec: "sudo": executable file not found` errors
- no repeated `NixOS cannot run dynamically linked executables` errors
- no repeated `certificate verify failed` from `osqueryd`

If you see repeated `certificate verify failed` plus
`Cannot read TLS server certificate(s): /opt/osquery/share/osquery/certs/certs.pem`,
apply latest config and restart Orbit:

```bash
sudo nixos-rebuild switch
sudo systemctl restart orbit.service
```
