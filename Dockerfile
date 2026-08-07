
#           BACKEND
FROM ubuntu:24.04 AS backend

# zlib1g-dev for haskell digest
RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y haskell-stack zlib1g-dev libtinfo-dev g++ build-essential

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

FROM ubuntu:24.04 AS frontend

RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y npm


WORKDIR /home/webdocs

COPY webdocs/package.json /home/webdocs/package.json
COPY webdocs/package-lock.json /home/webdocs/package-lock.json

RUN npm install

COPY webdocs .

RUN npm run build


#            RELEASE

FROM ubuntu:24.04

WORKDIR /home
COPY . .
COPY --from=backend /usr/local/bin/catln /usr/local/bin/catln
COPY --from=frontend /home/webdocs/dist /home/webdocs/dist
COPY stack /root/.catln/stack

# test/Integration/code is excluded until the CtService dep-scoping bug is fixed.
# buildSCCPipeline passes ALL pages as external deps rather than the transitive
# ones, so merging those 38 standalone programs collides on names they never
# import from each other: abs, Nothing, and main (defined by 37 of the 38).
# --readonly disables the editor's save endpoint so the shared docs site is immutable.
CMD ["catln", "doc", "--cached", "--readonly", "/root/.catln/stack", "docs"]
EXPOSE 8080