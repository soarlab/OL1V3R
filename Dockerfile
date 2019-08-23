FROM ubuntu:16.04
MAINTAINER Shaobo He <shaobo@cs.utah.edu>


RUN apt-get update && \
      apt-get -y install \
      software-properties-common \
      wget \
      sudo \
      unzip \
      git

# Borrowed from JFS
# Create `user` user for container with password `user`.  and give it
# password-less sudo access
RUN useradd -m user && \
    echo user:user | chpasswd && \
    cp /etc/sudoers /etc/sudoers.bak && \
    echo 'user  ALL=(root) NOPASSWD: ALL' >> /etc/sudoers

USER user

WORKDIR /home/user

# Install Racket
RUN wget https://mirror.racket-lang.org/installers/7.4/racket-7.4-x86_64-linux-cs.sh && \
    sudo sh racket-7.4-x86_64-linux-cs.sh --unix-style && \
    sudo apt-get install -y libmpfr-dev

# ``Install'' OL1V3R
RUN git clone https://github.com/soarlab/OL1V3R.git && \
    (cd OL1V3R && raco make main.rkt && git submodule init && git submodule update)

# Install Z3 
RUN wget https://github.com/Z3Prover/z3/releases/download/Z3-4.8.5/z3-4.8.5-x64-ubuntu-16.04.zip && \
    unzip z3-4.8.5-x64-ubuntu-16.04.zip && \
    mv z3-4.8.5-x64-ubuntu-16.04 z3-4.8.5 && \
    sudo apt-get install -y libgomp1
