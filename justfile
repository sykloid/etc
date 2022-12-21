export ANSIBLE_PYTHON_INTERPRETER := "auto_silent"
export DOCKER_BUILDKIT := "1"

# Do nothing; list available recipes.
help:
  just --list

# Provision the current home directory.
provision:
  ansible-playbook -i localhost, --connection=local ansible/main.yml

# Not sure this is ever going to be more general than it currently is,
# but oh well...
current_os := `docker info | sed -n -e 's/ OSType: \(.*\)/\1/p'`
current_arch := `docker info | sed -n -e 's/ Architecture: \(.*\)/\1/p'`
default := current_os + '/' + current_arch

# Build a new airlift docker image.
build ARCH=default:
    docker buildx build . --platform {{ ARCH }} --tag sykloid/airlift:latest --output type=docker

push ARCHES:
    docker buildx build --push . --platform {{ ARCHES }} --tag sykloid/airlift:latest
