
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
COPY --from=frontend /home/webdocs/build /home/webdocs/build

CMD ["catln", "doc", "--cached", "stack", "test/Integration/code", "docs"]
EXPOSE 8080