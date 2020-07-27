FROM ubuntu:18.04

ENV LANG=C.UTF-8
RUN apt-get update && apt-get install wget -y
RUN wget -qO- https://get.haskellstack.org/ | sh
RUN stack update
COPY . /src
RUN stack --stack-yaml /src/stack.yaml setup
RUN stack --stack-yaml /src/stack.yaml test --only-snapshot
