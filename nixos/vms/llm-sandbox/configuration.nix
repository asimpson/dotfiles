{ modulesPath, pkgs, ... }:

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

  services.openssh = {
    enable = true;
    settings = {
      KbdInteractiveAuthentication = false;
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      X11Forwarding = false;
    };
  };

  services.qemuGuest.enable = true;

  users.mutableUsers = false;
  users.users.agent = {
    isNormalUser = true;
    description = "Sandbox operator";
    extraGroups = [ "wheel" ];
    shell = pkgs.bashInteractive;
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
  ];

  system.activationScripts.npmsetup = {
    deps = [];
    text = ''
      cat > /home/agent/.npmrc <<'EOF'
      prefix=/home/agent/.npm-global
      EOF

      cat > /home/agent/.bash_profile <<'EOF'
      export PATH="$PATH:/home/agent/.npm-global/bin"
      alias claude="claude --dangerously-skip-permissions"
      alias codex="codex --dangerously-bypass-approvals-and-sandbox"
      EOF

      chown agent:users /home/agent/.npmrc /home/agent/.bash_profile
    '';
  };

  system.stateVersion = "25.11";
}
