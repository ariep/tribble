FROM ariep/tribble-base
MAINTAINER Arie Peterson <ariep@xs4all.nl>

# Run the rest of the statements as user ph.
USER ph

ENV LANG C.UTF-8

# Expose port 8000 and set workdir for CMD command.
EXPOSE 8000
WORKDIR /home/ph

# Copy stack and cabal files.
COPY ./stack.yaml /home/ph/stack.yaml
COPY ./client/stack.yaml /home/ph/client/stack.yaml
COPY ./common/tribble-common.cabal /home/ph/common/tribble-common.cabal
COPY ./server/tribble-server.cabal /home/ph/server/tribble-server.cabal
COPY ./client/tribble-client.cabal /home/ph/client/tribble-client.cabal
# Build the dependencies of the server.
ENV PATH /home/ph/.stack/programs/x86_64-linux/ghc-8.0.1/bin:$PATH
RUN stack build --only-dependencies
USER root
RUN chown ph:ph /home/ph/client
USER ph
# Build the dependencies of the client.
RUN stack build --only-dependencies --stack-yaml=client/stack.yaml

# Build the server.
ADD ./common/ /home/ph/common/
ADD ./server/ /home/ph/server/
USER root
RUN chown -R ph:ph /home/ph/common /home/ph/server
USER ph
RUN stack build

# Build the client.
ADD ./client/ /home/ph/client/
USER root
RUN chown -R ph:ph /home/ph/client
USER ph
RUN stack build --stack-yaml=client/stack.yaml
# Copy the generated javascript code to the directory served by the server.
RUN mkdir client/js && cp -f $(stack path --stack-yaml=client/stack.yaml --local-install-root)/bin/tribble-client.jsexe/* client/js/

# Work around docker not passing sigTERM to PID 1.
ADD ./my_init /
ENTRYPOINT ["/my_init","--skip-runit","--skip-startup-files"]

# Run when this container is started.
CMD ["/usr/bin/stack", "exec", "tribble-server"]
