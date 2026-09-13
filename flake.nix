{
  description = "Airlift";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Python packaging (uv2nix)
    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    uv2nix = {
      url = "github:pyproject-nix/uv2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
    };
    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.uv2nix.follows = "uv2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # MCP servers
    mcp-atlassian-src = {
      url = "github:sooperset/mcp-atlassian/v0.23.0";
      flake = false;
    };
  };

  outputs = { nixpkgs, home-manager, pyproject-nix, uv2nix, pyproject-build-systems, mcp-atlassian-src, ... }:
    let
      darwin = { config, pkgs, ... }: {
        _module.args.hostname = "atlantis";
        home.enableNixpkgsReleaseCheck = false;
        home.username = "sykloid";
        home.homeDirectory = "/Users/sykloid";
        home.stateVersion = "24.05";
        imports = [ definition ];
      };

      linux = { config, pkgs, ... }: {
        _module.args.hostname = "linux";
        home.enableNixpkgsReleaseCheck = false;
        home.username = "sykloid";
        home.homeDirectory = "/home/sykloid";
        home.stateVersion = "24.05";
        imports = [ definition ];
      };

      anl = { config, pkgs, ... }: {
        _module.args.hostname = "anl";
        home.enableNixpkgsReleaseCheck = false;
        home.username = "pcshyamshankar";
        home.homeDirectory = "/Users/pcshyamshankar";
        home.stateVersion = "24.05";
        imports = [ definition ];
      };

      definition = {pkgs, config, lib, hostname, ...}:
      let
        flakePath = builtins.getEnv "FLAKE_PATH";
        link = path:
          if flakePath != ""
          then config.lib.file.mkOutOfStoreSymlink "${flakePath}/${path}"
          else ./. + "/${path}";
        piBaseSettings = builtins.fromJSON (builtins.readFile ./pi/settings.json);
        piHostSettings = let path = ./. + "/pi/${hostname}.settings.json"; in
          if builtins.pathExists path then builtins.fromJSON (builtins.readFile path) else {};
        piSettings = (lib.recursiveUpdate piBaseSettings piHostSettings) // {
          packages = (piBaseSettings.packages or []) ++ (lib.subtractLists (piBaseSettings.packages or []) (piHostSettings.packages or []));
        };
        jsonFormat = pkgs.formats.json {};
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
          (import ./nixpkgs/mcp-atlassian.nix {
            inherit pkgs pyproject-nix uv2nix pyproject-build-systems mcp-atlassian-src;
          })
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
          ".config/ghostty/config".source = link "ghostty/config";

          ".pi/agent/settings.json".source = jsonFormat.generate "pi-settings.json" piSettings;
        }
        // (let path = ./. + "/pi/${hostname}.models.json"; in
          lib.optionalAttrs (builtins.pathExists path)
          { ".pi/agent/models.json".source = path; })
        // (let path = ./. + "/pi/${hostname}.mcp.json"; in
          lib.optionalAttrs (builtins.pathExists path)
          { ".pi/agent/mcp.json".source = path; });

        home.sessionVariables = { };

        news.display = "silent";
        programs.home-manager.enable = true;
      };
    in {
      homeConfigurations."atlantis" = home-manager.lib.homeManagerConfiguration {
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
