# so we can access the `pkgs` and `stdenv` variables
with import <nixpkgs> {};

let
  spago2nix = import (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "spago2nix";
      rev = "704fc193dd1066d3bee91e525ad5ea4876ad990e";
      sha256 = "1g82s3wz18lxif3pdd9nk6vb3c5cy1i1w5xpkl9gpvc44x8w7lrl";
    }) {};

  easy-ps = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "0ba91d9aa9f7421f6bfe4895677159a8a999bf20";
      sha256 = "1baq7mmd3vjas87f0gzlq83n2l1h3dlqajjqr7fgaazpa9xgzs7q";
    }
  ) {
    inherit pkgs;
  };

  node_packages = import ./node-nix-deps/default-node.nix { inherit (pkgs) nodejs; };
in
stdenv.mkDerivation {
  name = "purescript-bootstrap-shell";
  buildInputs = with pkgs; [
    python
    postgresql
    nodejs-12_x
    yarn
    yarn2nix
    easy-ps.purs
    easy-ps.spago
    easy-ps.spago2nix
    nodePackages.node2nix
    node_packages.purty
  ];
}
