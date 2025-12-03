FROM nixos/nix

# Install SBCL
RUN nix-env -iA nixpkgs.sbcl
RUN nix-env -iA nixpkgs.gnumake

# Move the whole project in.
WORKDIR /cli
COPY . .

# Start the build process.
WORKDIR /cli
RUN make install-ql
RUN make build

# Expose it.
ENV PATH="${PATH}:/cli/bin"
