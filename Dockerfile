# This file creates a container image for running and testing HasChor.
#
# Usage:
# Build the image:                   `docker build -t haschor .`
# Create a container and run Bash:   `docker run -it --name foo haschor bash`
# Run another bash in the container: `docker exec -it foo bash`
# Remove the container:              `docker rm foo`

FROM haskell:9.2.7

WORKDIR /HasChor

RUN cabal update

# Build dependencies
COPY ./HasChor.cabal .
RUN cabal build --only-dependencies -j20

# Build HasChor
COPY . .
RUN cabal build -j20
