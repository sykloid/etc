export ANSIBLE_PYTHON_INTERPRETER := "auto_silent"
export DOCKER_BUILDKIT := "1"

# Do nothing; list available recipes.
help:
  just --list

# Provision the current home directory.
provision:
  ansible-playbook -i localhost, --connection=local ansible/main.yml

# Build a new airlift docker image.
build:
  docker build . --tag sykloid/airlift:latest
