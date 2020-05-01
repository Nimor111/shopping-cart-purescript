with import <nixpkgs> {};

let
  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

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
in
mkYarnPackage rec {
  name = "shopping-cart";
  src = ./.;
  packageJSON = ./package.json;
  yarnLock = ./yarn.lock;

  nativeBuildInputs = [ easy-ps.purs nodejs-12_x ];

  postBuild = ''
    ${easy-ps.purs}/bin/purs compile "$src/**/*.purs" ${builtins.toString
      (builtins.map
        (x: ''"${x.outPath}/src/**/*.purs"'')
        (builtins.attrValues spagoPkgs.inputs))}
    '';

  postFixup = ''
    echo $PWD
    ls $out
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
