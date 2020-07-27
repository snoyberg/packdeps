FROM ubuntu:18.04 as build

RUN apt-get update && apt-get install wget -y
RUN wget -qO- https://get.haskellstack.org/ | sh
RUN stack update
RUN mkdir -p /src/packdeps-cli /src/packdeps-yesod
COPY stack.yaml /src/stack.yaml
COPY stack.yaml.lock /src/stack.yaml.lock
COPY packdeps-cli/packdeps.cabal /src/packdeps-cli/
COPY packdeps-yesod/packdeps-yesod.cabal /src/packdeps-yesod/
ENV LANG=C.UTF-8
RUN stack --stack-yaml /src/stack.yaml setup
RUN stack --stack-yaml /src/stack.yaml test --only-snapshot

COPY packdeps-cli /src/
COPY packdeps-yesod /src/
RUN stack --stack-yaml /src/stack.yaml install --test --local-bin-path /artifacts

FROM fpco/pid1:18.04

COPY --from=build /artifacts/packdeps-server /usr/local/bin/
COPY --from=build /src/packdeps-yesod/config/settings.yml /app/config/settings.yml
WORKDIR /app
CMD ["/usr/local/bin/packdeps-server", "production"]
