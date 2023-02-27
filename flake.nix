{
  description = "zmq";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/22.05";
    };
  };

  outputs = { self, nixpkgs }:
    {
      devShell = {
        x86_64-darwin =
          nixpkgs.legacyPackages.x86_64-darwin.mkShell {
            buildInputs = [
              nixpkgs.legacyPackages.x86_64-darwin.libsodium
              nixpkgs.legacyPackages.x86_64-darwin.pkg-config
              nixpkgs.legacyPackages.x86_64-darwin.zeromq
            ];
          };
        x86_64-linux =
          nixpkgs.legacyPackages.x86_64-linux.mkShell {
            buildInputs = [
              nixpkgs.legacyPackages.x86_64-linux.libsodium
              nixpkgs.legacyPackages.x86_64-linux.pkg-config
              nixpkgs.legacyPackages.x86_64-linux.zeromq
            ];
          };
      };
    };
}
