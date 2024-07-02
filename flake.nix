{
  description = "Airlift";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }:
    let
      darwin = { config, pkgs, ... }: {
        home.enableNixpkgsReleaseCheck = false;
        home.username = "sykloid";
        home.homeDirectory = "/Users/sykloid";
        home.stateVersion = "24.05";
        imports = [ definition ];
      };

      linux = { config, pkgs, ... }: {
        home.enableNixpkgsReleaseCheck = false;
        home.username = "sykloid";
        home.homeDirectory = "/home/sykloid";
        home.stateVersion = "24.05";
        imports = [ definition ];
      };

      definition = {pkgs, config, ...}: {
        home.packages = with pkgs; [
          bat
          direnv
          docker-client
          fd
          git
          just
          less
          man
          ncurses
          ripgrep
          tmux
          tree
          util-linux
          zellij
          zsh

          rye
        ];

        programs.emacs = {
          enable = true;
          package = pkgs.emacs-nox;
          extraPackages = epkgs: with epkgs; [
            backline
            bicycle
            breadcrumb
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
          ];
        };

        home.file = {
          ".emacs.d/init.el".source = ./emacs/init.el;
          ".emacs.d/skywave-theme.el".source = ./emacs/skywave-theme.el;

          ".tmux.conf".source = ./tmux/tmux.conf;
          ".config/zellij/config.kdl".source = ./zellij/config.kdl;

          ".zprofile".source = ./zsh/zprofile;
          ".zshrc".source = ./zsh/zshrc;
        };

        home.sessionVariables = { };

        programs.home-manager.enable = true;
      };
    in {
      homeConfigurations."darwin" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."aarch64-darwin";
        modules = [ darwin ];
      };

      homeConfigurations."linux" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."aarch64-linux";
        modules = [ linux ];
      };
    };
}
