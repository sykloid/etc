export ANSIBLE_PYTHON_INTERPRETER := "auto_silent"
export DOCKER_BUILDKIT := "1"

# Do nothing; list available recipes.
help:
  just --list

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

provision TARGET="${HOME}":
    mkdir -p {{ TARGET }}

    mkdir -p {{ TARGET }}/.config/nix
    cp nix/nix.conf {{ TARGET }}/.config/nix/nix.conf

    cp zsh/zprofile {{ TARGET }}/.zprofile
    cp zsh/zshrc {{ TARGET }}/.zshrc

    mkdir -p {{ TARGET }}/.terminfo
    tic -x -o {{ TARGET }}/.terminfo terminfo/xterm-24bit.terminfo

    mkdir -p {{ TARGET }}/.emacs.d
    cp emacs/init.el {{ TARGET }}/.emacs.d/init.el
    cp emacs/skywave-theme.el {{ TARGET }}/.emacs.d/skywave-theme.el

    cp tmux/tmux.conf {{ TARGET }}/.tmux.conf

    cp git/gitconfig {{ TARGET }}/.gitconfig
