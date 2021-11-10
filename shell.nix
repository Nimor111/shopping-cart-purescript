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
      rev = "7802db65618c2ead3a55121355816b4c41d276d9";
      sha256 = "0n99hxxcp9yc8yvx7bx4ac6askinfark7dnps3hzz5v9skrvq15q";
    }
  ) {
    inherit pkgs;
  };

  node_packages = import ./node-nix-deps { inherit (pkgs) nodejs; };
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
    node_packages.nodemon
  ];
}
