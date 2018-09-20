FROM fpco/stack-build:lts-12.2

ARG LTS_SLUG=lts-12.2

# setup node for building frontend
WORKDIR /

RUN curl -sL https://deb.nodesource.com/setup_8.x -o nodesource_setup.sh && \
      bash nodesource_setup.sh && \
      apt-get install -y nodejs

WORKDIR /moot

ENV HOME /moot
