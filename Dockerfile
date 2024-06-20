FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y && \
    apt-get install -y curl locales sudo && \
    rm -rf /var/lib/apt/lists/*

RUN useradd -m -U sykloid
RUN echo "sykloid ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
RUN dpkg-reconfigure --frontend=noninteractive locales && update-locale LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

RUN curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | \
  sh -s -- install linux \
  --extra-conf "sandbox = false" \
  --init none \
  --no-confirm
ENV PATH="${PATH}:/nix/var/nix/profiles/default/bin"

USER sykloid
ENV USER=sykloid

# In theory, this should not be necessary. We should be able to run home-manager
# below with sudo and have it populate our home directory. However, this appear
# to be broken.
RUN sudo chown -R sykloid: /nix

WORKDIR /home/sykloid
COPY --chown=sykloid . etc

RUN nix run home-manager/release-24.05 -- switch --flake /home/sykloid/etc#linux

ENV TERM=xterm-direct

CMD ${HOME}/.nix-profile/bin/zsh -l
