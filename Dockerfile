FROM ubuntu:xenial

RUN apt update \
	&& echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections \
	&& apt install -y \
		bzip2 \
		curl \
		git \
		perl \
		sudo \
		xz-utils

COPY nix.conf /etc/nix/nix.conf

RUN useradd -m -d /home/haskell -s /bin/bash haskell \
	&& usermod -a -G sudo haskell \
	&& echo " haskell      ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers \
	&& mkdir /haboom/ \
	&& chown haskell:haskell /haboom/

USER haskell
ENV USER=haskell
RUN curl -L https://nixos.org/nix/install | sh

COPY --chown=haskell:haskell ./reflex-platform/ /haboom/reflex-platform/
WORKDIR /haboom/

RUN bash -l reflex-platform/try-reflex \
	|| true # without this, installation does not complete. I have no idea why...

COPY --chown=haskell:haskell . /haboom/

ENTRYPOINT bash -l -c "reflex-platform/try-reflex --command '$0 $*'"
