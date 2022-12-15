{
  description = "Airlift";
  inputs.nixpkgs.url = github:NixOS/nixpkgs/22.05;

  outputs = { self, nixpkgs }: {

    defaultPackage.x86_64-linux = with import nixpkgs { system = "x86_64-linux"; }; buildEnv {
      name = "airlift";
      paths = [
          ansible
          bat
          direnv
          docker-client
          fd
          git
          just
          less
          man
          ncurses
          nixUnstable
          ripgrep
          tmux
          tree
          util-linux
          zsh

          ((emacsPackagesFor emacs-nox).emacsWithPackages
            (epkgs: with epkgs.melpaPackages; [
              eglot
              evil-args
              evil-commentary
              evil-surround
              evil-exchange
              expand-region
              company
              diminish
              direnv
              evil
              evil-surround
              general
              helm
              helm-projectile
              helm-rg
              magit
              magit-todos
              mwim
              no-littering
              outshine
              projectile
              undo-fu
              use-package
              which-key

              dockerfile-mode
              just-mode
              markdown-mode
              nix-mode
              rust-mode
              yaml-mode
            ])
          )
      ];
    };
  };
}
