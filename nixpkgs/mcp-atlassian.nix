# Requires flake inputs: pyproject-nix, uv2nix, pyproject-build-systems, mcp-atlassian-src
{ pkgs, pyproject-nix, uv2nix, pyproject-build-systems, mcp-atlassian-src }:

let
  python = pkgs.python312;

  workspace = uv2nix.lib.workspace.loadWorkspace {
    workspaceRoot = mcp-atlassian-src;
  };

  overlay = workspace.mkPyprojectOverlay {
    sourcePreference = "wheel";
  };

  pythonSet =
    (pkgs.callPackage pyproject-nix.build.packages { inherit python; }).overrideScope
      (pkgs.lib.composeManyExtensions [
        pyproject-build-systems.overlays.default
        overlay
      ]);

in
pythonSet.mkVirtualEnv "mcp-atlassian-env" workspace.deps.default
