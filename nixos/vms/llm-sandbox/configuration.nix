{ modulesPath, lib, pkgs, ... }:

let
  pat = import ./pat.nix;
  md = builtins.readFile ./AGENTS.md;
  zshrc = builtins.readFile "/home/adam/.zshrc";
  agents = pkgs.writeText "AGENTS.md" ''${md}'';
  claude = pkgs.writeText "CLAUDE.md" ''@AGENTS.md'';
  npmPrefix = "/home/agent/.npm-global";
  home = "/home/agent";
  npmGlobals = [
      "@anthropic-ai/claude-code"
      "@openai/codex"
  ];
in
{
  imports = [
    "${modulesPath}/profiles/minimal.nix"
    "${modulesPath}/profiles/qemu-guest.nix"
  ];

  networking = {
    hostName = "llm-jail";
    #set a hosts entry for a specific IP in libvirt
    useDHCP = true;
    firewall = {
      enable = false;
    };
  };

  #ssh-keygen -t ed25519 -f vm-host-key -N ""
  environment.etc."ssh/ssh_host_ed25519_key" = {
    source = ./ssh/vm-host-key;
    mode = "0600";
  };
  environment.etc."ssh/ssh_host_ed25519_key.pub" = {
    source = ./ssh/vm-host-key.pub;
    mode = "0644";
  };

  services.openssh = {
    enable = true;
    settings = {
      KbdInteractiveAuthentication = false;
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      X11Forwarding = false;
    };
    hostKeys = [
      {
        path = "/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
    ];
  };

  programs.zsh.enable = true;

  services.qemuGuest.enable = true;

  users.mutableUsers = false;
  users.users.agent = {
    isNormalUser = true;
    description = "Sandbox operator";
    extraGroups = [ "wheel" ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDJPyPjtPWFcMjjaeW3ts3nfBOYRHnZkyGhgcOUr/gvGv/z1xb2KyrfXgpTIfWU7b5ZHNoS28c/s7gqMAlylROe/N4dYQXeORxuuTO473fdveuUgRoHAu6NlMzKWuELI9KwOyASQibQt9GS0M41bVHHg33JxQhyM5jbif06ynW8qSsT8h7xM+wvq3YD+4IDZcIPOKn4AlRxOc5FHauJMaLwQMMk6RcFKUcxOUDdqMwC2xb5t1OKEM/2A686OopXZY21QuYJoWBrXLxFVpfCdf+2/6+2CnFDaxwv+oLbrZRnsJglIGhMN4T8gj4AnyKniIUMMMHFkwDyLbmjxzrhFNU5MTSKUtx1k4b6+Z2hkMgkf3biyZW1rafxZ4B/6TJxFPxWzF4ayTdf+Yu27n4Z5kx44jiojfDaUSTp0mg3w2Gf1ozcwW4KHMjGPtIUs3U71ZtFsxliZMY5/7osRAwt/kwuLGx9YqWz+bmoPUEcp9YHukw+Pv3hUPxNKVIA42E+J+0= adam@nix-grafana"
    ];
  };

  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    bashInteractive
    curl
    git
    htop
    btop
    jq
    ripgrep
    rsync
    tmux
    vim
    nodejs
    go
    gopls
    gh
    fzf
  ];

  system.activationScripts.npmsetup = {
    deps = [];
    text = ''
      cat > ${home}/.npmrc <<'EOF'
      prefix=${npmPrefix}
      EOF

      cat > ${home}/.zshrc <<'EOF'
      ${zshrc}
      HISTFILE=/persist/zsh/history
      export PATH="$PATH:${npmPrefix}/bin"
      export GH_TOKEN=${pat.ro}
      alias claude="claude --dangerously-skip-permissions"
      alias codex="codex --dangerously-bypass-approvals-and-sandbox"
      precmd() {
        PROMPT="%~ %{$fg[yellow]%}=jail=%{$reset_color%} "
        PROMPT+="%D{%I:%M}%{$fg[magenta]%}❯ %{$reset_color%}"
      }
      EOF

      chown agent:users ${home}/.npmrc ${home}/.bash_profile
    '';
  };

  system.activationScripts.agentMarkdown = {
    deps = [];
    text = ''
      install -Dm644 ${agents} ${home}/AGENTS.md
      install -Dm644 ${claude} ${home}/CLAUDE.md
      chown agent:users ${home}/*.md
    '';
  };

  systemd.services.agent-npm-globals = {
    description = "Install claude and codex";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    path = [ pkgs.nodejs ];

    serviceConfig = {
      Type = "oneshot";
      User = "agent";
      Group = "users";
      WorkingDirectory = home;
      Restart = "on-failure";
      RestartSec = "10s";
      Environment = [
        "HOME=${home}"
        "NPM_CONFIG_PREFIX=${npmPrefix}"
        "PATH=${npmPrefix}/bin:/run/current-system/sw/bin"
      ];
    };

    script = ''npm install -g ${lib.escapeShellArgs npmGlobals}'';
  };

  # created an disk on the host: qemu-img create -f qcow2 ~/llm-jail-persist 1G
  # mounted that into the vm via virtual machine manager
  # in the VM: mkfs.ext4 -L persist /dev/vdb
  fileSystems."/persist" = {
    device = "/dev/disk/by-label/persist";
    fsType = "ext4";
    neededForBoot = true;
  };

  systemd.tmpfiles.rules = [
    "L+ ${home}/.codex - - - - /persist/.codex"
    "L+ ${home}/.claude - - - - /persist/.claude"
    "L+ ${home}/.claude.json - - - - /persist/.claude.json"
    "d /mnt - - - -"
  ];

  system.stateVersion = "25.11";
}
