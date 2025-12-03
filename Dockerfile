FROM nixos/nix

# Install SBCL
RUN nix-env -iA nixpkgs.sbcl

# Move the whole project in.
WORKDIR /cli
COPY . .

# Install package dependencies.
WORKDIR /cli
RUN make build

# Expose it.
ENV PATH="${PATH}:/cli/bin"
