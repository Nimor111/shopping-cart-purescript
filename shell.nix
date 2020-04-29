# so we can access the `pkgs` and `stdenv` variables
with import <nixpkgs> {};

let
  spago2nix = import (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "spago2nix";
      rev = "262020b1bae872dac6db855fafe58a9999c91a28";
      sha256 = "0l678qjb73f1kvkk3l1pby2qg272dj166yxl7b1mcb0xhnjgig7g";
    }) {};

  easy-ps = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "aa3e608608232f4a009b5c132ae763fdabfb4aba";
      sha256 = "0y6jikncxs9l2zgngbd1775f1zy5s1hdc5rhkyzsyaalcl5cajk8";
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
