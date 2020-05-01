with import <nixpkgs> {};

let
  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

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
