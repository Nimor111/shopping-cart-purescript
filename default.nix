with import <nixpkgs> {};

let
  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

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

in
mkYarnPackage rec {
  name = "shopping-cart";
  src = ./.;
  packageJSON = ./package.json;
  yarnLock = ./yarn.lock;

  nativeBuildInputs = [ easy-ps.purs nodejs-14_x ];

  postBuild = ''
    ${easy-ps.purs}/bin/purs compile "$src/**/*.purs" ${builtins.toString
      (builtins.map
        (x: ''"${x.outPath}/src/**/*.purs"'')
        (builtins.attrValues spagoPkgs.inputs))}
    '';

  postFixup = ''
    ${easy-ps.spago}/bin/spago bundle-app --no-install \
      --no-build --main Main --to dist/app.js
    mkdir -p $out/dist
    cp -r dist $out/
    ln -s $out/libexec/${name}/node_modules $out/dist
  '';

  meta = with stdenv.lib; {
    description = "Shopping cart application in PureScript.";
    homepage = "https://github.com/Nimor111/shopping-cart";
    maintainers = with maintainers; [ Nimor111 ];
  };
}
