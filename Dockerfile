FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y curl git locales sudo xz-utils \
    build-essential libssl-dev zlib1g-dev libbz2-dev \
    libreadline-dev libsqlite3-dev curl llvm libncursesw5-dev \
    tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev \
    libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev \
    libncurses5 libnuma1 libtinfo5 && rm -rf /var/lib/apt/lists/*

RUN useradd -m -U sykloid
RUN echo "sykloid ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
RUN dpkg-reconfigure --frontend=noninteractive locales && update-locale LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

RUN mkdir -m 0755 /nix && chown sykloid /nix

USER sykloid

RUN curl -L https://nixos.org/nix/install | sh

RUN mkdir -p ~/.config/nix && echo 'experimental-features = nix-command flakes' > ~/.config/nix/nix.conf

COPY flake.nix /airlift/flake.nix
COPY flake.lock /airlift/flake.lock

WORKDIR /airlift

RUN rm -rf ~/.cache/nix
RUN PATH=/home/sykloid/.nix-profile/bin:$PATH nix build --no-link --profile /nix/var/nix/profiles/airlift

ENV AIRLIFT=/nix/var/nix/profiles/airlift
ENV PATH=${AIRLIFT}/bin:${PATH}
ENV TERM=screen-256color

ADD --chown=sykloid:sykloid . /airlift

WORKDIR /home/sykloid
RUN just /airlift/provision

# * Toolchains
# ** Haskell: ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK_HOOK=1 \
    sh

# ** Rust: rustup
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain none
RUN rustup completions zsh rustup > /home/sykloid/.zfunc/_rustup
RUN rustup completions zsh cargo > /home/sykloid/.zfunc/_cargo

# ** Python: rye
RUN curl -sSf https://rye-up.com/get | RYE_INSTALL_OPTION="--yes" bash
RUN rye self completion -s zsh > ~/.zfunc/_rye

CMD ${AIRLIFT}/bin/zsh -l
