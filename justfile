export ANSIBLE_PYTHON_INTERPRETER := "auto_silent"
export DOCKER_BUILDKIT := "1"

provision:
  ansible-playbook -i localhost, --connection=local ansible/main.yml

build:
  docker build . --target stage-1 --tag sykloid/airlift:latest
