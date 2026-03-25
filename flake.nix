{
  description = "Airlift";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
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

      anl = { config, pkgs, ... }: {
        home.enableNixpkgsReleaseCheck = false;
        home.username = "pcshyamshankar";
        home.homeDirectory = "/Users/pcshyamshankar";
        home.stateVersion = "24.05";
        imports = [ definition ];
      };

      definition = {pkgs, config, lib, ...}:
      let
        flakePath = builtins.getEnv "FLAKE_PATH";
        link = path:
          if flakePath != ""
          then config.lib.file.mkOutOfStoreSymlink "${flakePath}/${path}"
          else ./. + "/${path}";
      in {
        home.packages = with pkgs; [
          bat
          # direnv
          fd
          gh
          glab
          git
          just
          man
          ncurses
          nushell
          ripgrep
          tree
          # util-linux
          zellij
          zsh
          jsonnet-language-server
          luau-lsp
          gopls
          texlab
          typescript-language-server
          (callPackage ./nixpkgs/pi.nix { })
        ];

        programs.emacs = {
          enable = true;
          package = pkgs.emacs-nox;
        };

        home.file = {
          ".config/emacs/early-init.el".source = ./emacs/early-init.el;
          ".config/emacs/init.el".source = ./emacs/init.el;
          ".config/emacs/elpaca-bootstrap.el".source = ./emacs/elpaca-bootstrap.el;
          ".config/emacs/skywave-theme.el".source = ./emacs/skywave-theme.el;

          ".tmux.conf".source = ./tmux/tmux.conf;
          ".config/zellij/config.kdl".source = ./zellij/config.kdl;

          ".zprofile".source = ./zsh/zprofile;
          ".zshrc".source = ./zsh/zshrc;

          "Library/Application Support/nushell/config.nu".source = ./nushell/config.nu;

          ".config/wezterm/wezterm.lua".source = link "wezterm/wezterm.lua";
        };

        home.sessionVariables = { };

        news.display = "silent";
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

      homeConfigurations."anl" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."aarch64-darwin";
        modules = [ anl ];
      };
    };
}
