FROM alpine

RUN apk update
RUN apk add gcc git uutils shadow nushell \
    cargo rust-src rust-clippy rustfmt
RUN chsh -s /usr/bin/nu