
FROM ubuntu:24.04

RUN apt-get update -y && apt-get upgrade -y

# zlib1g-dev for haskell digest
RUN apt-get install -y haskell-stack npm zlib1g-dev libtinfo-dev

# Fix for pandoc build https://stackoverflow.com/questions/63746826/what-might-cause-commitandreleasebuffer-invalid-argument-invalid-character
ENV LANG=C.UTF-8

WORKDIR /home

# Setup haskell depdencies first for docker build cache
COPY stack.yaml /home/stack.yaml
COPY package.yaml /home/package.yaml
RUN stack build --only-dependencies

COPY . .

RUN stack install --local-bin-path=/usr/local/bin
RUN cd webdocs && npm install && npm run build

CMD ["catln", "doc", "--cached", "stack", "test/Integration/code"]
EXPOSE 8080