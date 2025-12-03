{
  description = "Development shell for Common Lisp with Qob";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      # Define the systems you want to support
      systems = [
        "x86_64-linux"
        "i686-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      {
        # Generate devShells for each system
        devShells = nixpkgs.lib.genAttrs systems (system:
          let
            # Get the nixpkgs set for the current system
            pkgs = nixpkgs.legacyPackages.${system};
          in
            {
              # Define the default development shell for the system
              default = pkgs.mkShell {
                buildInputs = [
                  pkgs.sbcl
                  pkgs.gnumake
                ];

                shellHook = ''
                    echo "Welcome to the Common Lisp development shell with Qob!"

                    # Export `bin` to PATH.
                    export PATH="$PATH:$PWD/bin"

                    # Install if `bin/qob` is missing
                    if [ ! -e bin/qob ]; then
                      make install-ql-no-network
                      make build
                    fi
                    '';
              };
            }
        );
      };
}
