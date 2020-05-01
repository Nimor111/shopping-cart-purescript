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
      rev = "d4879bfd2b595d7fbd37da1a7bea5d0361975eb3";
      sha256 = "0kzwg3mwziwx378kvbzhayy65abvk1axi12zvf2f92cs53iridwh";
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
