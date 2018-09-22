FROM ubuntu:18.04
# FROM fpco/stack-build:lts-12.2

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH=/moot/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN apt-get update && \
      apt-get install -y wget curl gnupg2 postgresql-server-dev-all postgresql-client && \
      curl -sSL https://get.haskellstack.org/ | sh && \
      rm -rf /var/lib/apt/lists/*

# setup node for building frontend
WORKDIR /

RUN curl -sL https://deb.nodesource.com/setup_8.x -o nodesource_setup.sh && \
      bash nodesource_setup.sh && \
      apt-get install -y nodejs

WORKDIR /moot

ENV HOME /moot
