
#           BACKEND
FROM ubuntu:24.04 AS backend

# zlib1g-dev for haskell digest
# Stack comes from its official installer rather than apt: the apt package (2.9) predates Hackage's
# current root keys and fails to verify the package index ("root.json does not have enough
# signatures").
RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y curl zlib1g-dev libtinfo-dev libgmp-dev g++ build-essential && \
  curl -sSL https://get.haskellstack.org/ | sh

# Fix for pandoc build https://stackoverflow.com/questions/63746826/what-might-cause-commitandreleasebuffer-invalid-argument-invalid-character
ENV LANG=C.UTF-8

WORKDIR /home

# Setup haskell depdencies first for docker build cache
COPY stack.yaml /home/stack.yaml
COPY package.yaml /home/package.yaml
RUN stack build --only-dependencies

COPY . .
RUN stack install --local-bin-path=/usr/local/bin

#           FRONTEND

# npm 12, which wrote the lockfile, needs Node 22.22 or later.
FROM node:24 AS frontend

RUN npm install -g npm@12

WORKDIR /home/webdocs

COPY webdocs/package.json /home/webdocs/package.json
COPY webdocs/package-lock.json /home/webdocs/package-lock.json

RUN npm ci

COPY webdocs .

RUN npm run build


#            RELEASE

FROM ubuntu:24.04

# The library and docs are read as UTF-8, which the default locale cannot decode.
ENV LANG=C.UTF-8

# The repository, for the docs and the files they link to.
COPY . /home/catln
COPY --from=backend /usr/local/bin/catln /usr/local/bin/catln
COPY stack /root/.catln/stack

# Run from a directory of its own holding only the webdocs bundle, which the server serves relative
# to where it runs. A compiler run where a stack/core directory is uses that as its library
# (Semantics.buildCtssConfig), so running from the repository would serve its copy of the library
# beside /root/.catln/stack rather than the one installed for it.
WORKDIR /srv/catln
COPY --from=frontend /home/webdocs/dist /srv/catln/webdocs/dist

# Serves the library and its docs. Other pages can be served beside them by
# adding them as roots, since the service builds each page against only the
# pages it imports (see webdocs-serve in the Makefile).
# --readonly disables the editor's save endpoint so the shared docs site is immutable.
CMD ["catln", "doc", "--readonly", "/root/.catln/stack", "/home/catln/docs"]
EXPOSE 8080
