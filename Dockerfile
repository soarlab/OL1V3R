FROM ubuntu:22.04
MAINTAINER Shaobo He <shaobo@cs.utah.edu>

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
      apt-get -y install \
      software-properties-common \
      wget \
      sudo \
      unzip \
      git \
      libmpfr-dev \
      libgomp1

# Borrowed from JFS
# Create `user` user for container with password `user`.  and give it
# password-less sudo access
RUN useradd -m user && \
    echo "user:user" | chpasswd && \
    cp /etc/sudoers /etc/sudoers.bak && \
    echo 'user  ALL=(root) NOPASSWD: ALL' >> /etc/sudoers

USER user
WORKDIR /home/user

# Install Racket
RUN wget https://mirror.racket-lang.org/installers/9.2/racket-9.2-x86_64-linux-cs.sh && \
    sudo sh racket-9.2-x86_64-linux-cs.sh --unix-style && \
    rm racket-9.2-x86_64-linux-cs.sh

# Install Z3 
RUN wget https://github.com/Z3Prover/z3/releases/download/z3-4.13.0/z3-4.13.0-x64-glibc-2.35.zip && \
    unzip z3-4.13.0-x64-glibc-2.35.zip && \
    mv z3-4.13.0-x64-glibc-2.35 z3 && \
    rm z3-4.13.0-x64-glibc-2.35.zip

ENV PATH="/home/user/z3/bin:${PATH}"

# ``Install'' OL1V3R
RUN git clone https://github.com/soarlab/OL1V3R.git && \
    (cd OL1V3R && raco make main.rkt && git submodule init && git submodule update)
