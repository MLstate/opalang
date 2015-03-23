# Generic Opa container

FROM phusion/baseimage:0.9.16

MAINTAINER MLstate <contact@mlstate.com>

# Install stuff we need
RUN add-apt-repository ppa:avsm/ppa
RUN apt-get update && apt-get install -y \
  ocaml opam camlp4 camlp4-extra \
  nodejs make openjdk-7-jre m4 \
  zlib1g-dev unzip git
RUN opam init
RUN opam install -y ulex camlzip ocamlgraph ocamlfind

# Download and build opa
RUN git clone https://github.com/MLstate/opalang
RUN eval `opam config env` && cd opalang && ./configure -ocamlfind `which ocamlfind` && make && make install
