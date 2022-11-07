FROM amd64/ubuntu:22.04

RUN apt-get update && apt-get install -y curl git locales sudo xz-utils && rm -rf /var/lib/apt/lists/*

RUN useradd -m -U sykloid
RUN echo "sykloid ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
RUN dpkg-reconfigure --frontend=noninteractive locales && update-locale LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

RUN mkdir -m 0755 /nix && chown sykloid /nix

USER sykloid

RUN curl -L https://github.com/numtide/nix-unstable-installer/releases/download/nix-2.6.0pre20211223_af553b2/install | sh

RUN mkdir -p ~/.config/nix && echo 'experimental-features = nix-command flakes' > ~/.config/nix/nix.conf

ADD --chown=sykloid:sykloid . /airlift
WORKDIR /airlift

RUN rm -rf ~/.cache/nix
RUN PATH=/home/sykloid/.nix-profile/bin:$PATH nix build --profile /nix/var/nix/profiles/airlift

ENV AIRLIFT=/nix/var/nix/profiles/airlift
ENV PATH=${AIRLIFT}/bin:${PATH}
ENV TERM=screen-256color

RUN ${AIRLIFT}/bin/ansible-playbook -i localhost, --connection=local /airlift/ansible/main.yml

CMD $AIRLIFT/bin/zsh -l
