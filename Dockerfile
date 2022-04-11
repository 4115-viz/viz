# Based on 20.04 LTS
FROM ubuntu:focal
ENV TZ=America/New_York
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get -yq update && \
    apt-get -y upgrade && \
    apt-get -yq --no-install-suggests --no-install-recommends install \
    ocaml \
    menhir \
    llvm-11 \
    llvm-11-dev \
    m4 \
    git \
    aspcud \
    ca-certificates \
    python2.7 \
    pkg-config \
    cmake \
    opam

RUN ln -s /usr/bin/lli-11 /usr/bin/lli
RUN ln -s /usr/bin/llc-11 /usr/bin/llc
RUN opam init --auto-setup --yes --disable-sandboxing

WORKDIR '/viz'
COPY ./viz.opam .
RUN opam install . --deps-only --yes

WORKDIR /root

ENTRYPOINT ["opam", "config", "exec", "--"]

CMD ["bash"]