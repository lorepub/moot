FROM fpco/stack-build:lts-12.2

ARG LTS_SLUG=lts-12.2

RUN stack --system-ghc --resolver=$LTS_SLUG --local-bin-path=/usr/local/bin install \
        yesod-bin && \
    cd $HOME/.stack && \
    find . -type f -not -path './snapshots/*/share/*' -exec rm '{}' \; && \
    find . -type d -print0 |sort -rz |xargs -0 rmdir 2>/dev/null || true

WORKDIR /moot

COPY . .

RUN stack build

CMD ["/usr/bin/make", "backend-watch"]