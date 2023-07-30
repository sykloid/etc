{
  description = "Airlift";
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-23.05;

  outputs = { self, nixpkgs }:
    let output = system: with import nixpkgs { system = system; }; buildEnv {
      name = "airlift";
      paths = [
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

          ((emacsPackagesFor emacs29-nox).emacsWithPackages
            (epkgs: with epkgs.melpaPackages; [
              backline
              company
              diminish
              direnv
              epkgs.rainbow-mode
              evil
              evil-args
              evil-commentary
              evil-exchange
              evil-surround
              evil-surround
              expand-region
              forge
              general
              magit
              magit-todos
              mwim
              no-littering
              outline-minor-faces
              undo-fu
              use-package
              which-key

              consult
              embark
              epkgs.embark-consult
              epkgs.orderless
              epkgs.vertico
              marginalia

              dockerfile-mode
              haskell-mode
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
