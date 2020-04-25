{ pkgs ? (fetchTarball channel:nixos-19.09) }:

with import pkgs {}; buildEnv {
  name = "airlift";
  paths = [
    ansible
    bat
    ((emacsPackagesFor emacs26-nox).emacsWithPackages
      (epkgs: with epkgs; [
        evil
        general
        magit
        org-plus-contrib
        outshine
        use-package
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
