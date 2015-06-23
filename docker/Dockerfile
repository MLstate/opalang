# Generic Opa container
#
# Build
#
# docker build -t mlstate/opa .
#
# Mount local folder as volume and create a 'chat' app:
#
# docker run --rm -v `pwd`:/src -ti mlstate/opa create chat

FROM phusion/baseimage:0.9.16

MAINTAINER MLstate <contact@mlstate.com>

# Install stuff we need
RUN add-apt-repository ppa:avsm/ppa
RUN apt-get update && apt-get install -y \
  ocaml opam camlp4 camlp4-extra \
  nodejs npm make openjdk-7-jre m4 \
  zlib1g-dev unzip git
RUN opam init
RUN opam install -y ulex camlzip ocamlgraph ocamlfind

# Download and build opa
RUN git clone https://github.com/MLstate/opalang
RUN eval `opam config env` && cd opalang && ./configure -ocamlfind `which ocamlfind` && make && make install
RUN npm install -g intl-messageformat intl

VOLUME ["/src"]
WORKDIR /src

ENTRYPOINT ["/usr/local/bin/opa"]
