# Do nothing; list available recipes.
help:
  just --list

switch:
  nix run home-manager/release-25.11 -- switch --flake ~/Projects/etc#darwin --show-trace
