FROM nixos/nix:2.3 AS BASE

COPY airlift.nix /tmp/airlift.nix
RUN nix build -f /tmp/airlift.nix -o /opt/nix/var/nix/profiles/airlift
RUN nix copy --no-check-sigs -f /tmp/airlift.nix --to local?root=/opt

FROM ubuntu:20.04

COPY --from=BASE /opt/nix /nix

ENV AIRLIFT=/nix/var/nix/profiles/airlift
ENV LANG en_US.UTF-8
ENV TERM=xterm-direct2

USER root
WORKDIR /root

COPY . etc

RUN $AIRLIFT/bin/ansible-playbook -i localhost, --connection=local etc/ansible/main.yml
CMD $AIRLIFT/bin/zsh -l
