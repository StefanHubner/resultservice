#FROM haskell:latest
#FROM ubuntu:latest
FROM debian:bookworm-slim

# Install PostgreSQL server
RUN apt-get update && apt-get install -y postgresql postgresql-contrib \
    libpq-dev build-essential postgresql-server-dev-all \
    liblz4-dev libpam0g-dev netcat-traditional sudo cron awscli  \
    curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev \
    libncurses5 libtinfo5  libsqlite3-dev zlib1g-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN curl -sSfL "https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup" -o x86_64-linux-ghcup
RUN chmod +x x86_64-linux-ghcup
RUN ./x86_64-linux-ghcup install stack 2.9.3
RUN ./x86_64-linux-ghcup set stack 2.9.3

RUN service postgresql start

WORKDIR /app

COPY . .
COPY .config/credentials /root/.aws/
COPY .config/.pgpass /root/

RUN /root/.ghcup/bin/stack build --install-ghc --only-dependencies
RUN /root/.ghcup/bin/stack install

RUN /root/.ghcup/bin/stack clean --full

ENV PATH="/root/.local/bin:${PATH}"

EXPOSE 8080

COPY ./entrypoint.sh /usr/local/bin/
COPY ./backup.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/backup.sh

CMD ["entrypoint.sh"]
