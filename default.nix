with import <nixpkgs> {};

let
  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

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
    #${easy-ps.spago}/bin/spago bundle-app --no-install \
     #--no-build --main Main --to dist/app.js
    # FIXME use command above when this issue is resolved: https://github.com/purescript/spago/issues/634
    ${easy-ps.purs}/bin/purs bundle output/*/*.js -m Main --main Main -o dist/app.js
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
