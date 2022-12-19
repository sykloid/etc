export ANSIBLE_PYTHON_INTERPRETER := "auto_silent"
export DOCKER_BUILDKIT := "1"

# Do nothing; list available recipes.
help:
  just --list

# Provision the current home directory.
provision:
  ansible-playbook -i localhost, --connection=local ansible/main.yml

# Build a new airlift docker image.
build-docker ARCH:
  docker build --build-arg ARCH={{ ARCH }} . --tag sykloid/airlift:arm64

build-amd64:
    just build-docker amd64

build-arm64:
    just build-docker arm64v8
