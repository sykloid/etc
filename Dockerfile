FROM nixpkgs/nix-unstable:nixos-20.09 AS stage-0

RUN mkdir -p ~/.config/nix && \
    echo 'experimental-features = nix-command flakes' > ~/.config/nix/nix.conf

COPY . /airlift
WORKDIR /airlift

RUN nix build --profile /nix/var/nix/profiles/airlift

FROM ubuntu:20.04 AS stage-1

RUN useradd -m -U sykloid
RUN mkdir -p ~/.config/nix && \
    echo 'experimental-features = nix-command flakes' > ~/.config/nix/nix.conf

RUN apt update && apt -y install curl locales sudo

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
RUN dpkg-reconfigure --frontend=noninteractive locales && update-locale LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

COPY --from=stage-0 --chown=sykloid:sykloid /airlift /airlift
COPY --from=stage-0 --chown=sykloid:sykloid /nix /nix

ENV AIRLIFT=/nix/var/nix/profiles/airlift
ENV PATH=${AIRLIFT}/bin:${PATH}

ENV TERM=screen-256color

RUN echo "sykloid ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER sykloid
WORKDIR /home/sykloid

CMD $AIRLIFT/bin/zsh -l

FROM stage-1 AS stage-2

RUN ${AIRLIFT}/bin/ansible-playbook -i localhost, --connection=local /airlift/ansible/main.yml
