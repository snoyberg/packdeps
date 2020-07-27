FROM ubuntu:18.04 as build

ENV LANG=C.UTF-8
RUN apt-get update && apt-get install wget -y
RUN wget -qO- https://get.haskellstack.org/ | sh
RUN stack update
COPY . /src
RUN stack --stack-yaml /src/stack.yaml setup
RUN stack --stack-yaml /src/stack.yaml test --only-snapshot
RUN stack --stack-yaml /src/stack.yaml install --test --local-bin-path /artifacts

FROM fpco/pid1:18.04

COPY --from=build /artifacts/packdeps-server /usr/local/bin/
COPY --from=build /src/packdeps-yesod/config/settings.yml /app/config/settings.yml
WORKDIR /app
CMD ["/usr/local/bin/packdeps-server", "production"]
