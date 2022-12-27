{
  description = "Airlift";
  inputs.nixpkgs.url = github:NixOS/nixpkgs/22.11;

  outputs = { self, nixpkgs }:
    let output = system: with import nixpkgs { system = system; }; buildEnv {
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
              magit
              magit-todos
              mwim
              no-littering
              outshine
              undo-fu
              use-package
              which-key

              consult
              embark
              marginalia
              epkgs.vertico
              epkgs.orderless
              epkgs.embark-consult

              dockerfile-mode
              just-mode
              markdown-mode
              nix-mode
              rust-mode
              yaml-mode
            ])
          )
      ];
    }; in {
      defaultPackage.aarch64-linux = output "aarch64-linux";
      defaultPackage.x86_64-linux = output "x86_64-linux";
    };
}
