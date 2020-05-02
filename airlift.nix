{ pkgs ? (builtins.fetchTarball https://github.com/NixOS/nixpkgs/tarball/fce7562cf46727fdaf801b232116bc9ce0512049) }:

with import pkgs {}; buildEnv {
  name = "airlift";
  paths = [
    ansible
    bat
    ((emacsPackagesFor emacs26-nox).emacsWithPackages
      (epkgs: with epkgs; [
        company
        diminish
        evil
        general
        helm
        magit
        org-plus-contrib
        outshine
        use-package
        which-key
      ])
    )
    git
    less
    man
    ripgrep
    tmux
    zsh
  ];
}
