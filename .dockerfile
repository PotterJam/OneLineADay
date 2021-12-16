FROM haskell:8.10.7

WORKDIR /opt/live-lines

COPY ./live-lines.cabal /ops/live-lines/live-lines.cabal
RUN cabal build --only-dependencies -j4

COPY . /opt/live-lines
RUN cabal install

CMD ["live-lines"]