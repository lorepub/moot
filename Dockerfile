FROM ubuntu:18.04

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN apt-get update && \
      apt-get install -y wget curl gnupg2 postgresql-server-dev-all postgresql-client && \
      curl -sSL https://get.haskellstack.org/ | sh && \
      rm -rf /var/lib/apt/lists/*

RUN stack setup --resolver lts-12.2

RUN mkdir -p /builds/lorepub/moot
WORKDIR /builds/lorepub/moot

RUN curl -sL https://deb.nodesource.com/setup_9.x -o nodesource_setup.sh && \
      bash nodesource_setup.sh && \
      apt-get install -y nodejs

COPY frontend/package.json package.json

RUN npm install -g gulp
