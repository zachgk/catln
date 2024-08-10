
FROM ubuntu:24.04

RUN apt-get update -y && apt-get upgrade -y

# zlib1g-dev for haskell digest
RUN apt-get install -y haskell-stack npm zlib1g-dev libtinfo-dev

WORKDIR /home
COPY . .

# Fix for pandoc build https://stackoverflow.com/questions/63746826/what-might-cause-commitandreleasebuffer-invalid-argument-invalid-character
ENV LANG=C.UTF-8

# Fix for local building
RUN git clean -Xdf

RUN stack install --local-bin-path=/usr/local/bin
RUN cd webdocs && npm install && npm run build

CMD ["catln", "doc", "test/Integration/code", "--cached"]
EXPOSE 8080