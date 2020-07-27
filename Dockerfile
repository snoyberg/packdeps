FROM docker.pkg.github.com/snoyberg/packdeps/base:5fa3d2479b9204a321cd12642817902ef32143f2 as build

RUN rm -rf /src
COPY . /src
RUN stack --stack-yaml /src/stack.yaml install --test --local-bin-path /artifacts

FROM fpco/pid1:18.04

COPY --from=build /artifacts/packdeps-server /usr/local/bin/
COPY --from=build /src/packdeps-yesod/config/settings.yml /app/config/settings.yml
WORKDIR /app
CMD ["/usr/local/bin/packdeps-server", "production"]
