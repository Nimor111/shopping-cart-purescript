# shopping-cart-purescript

[![Build Status](https://travis-ci.com/Nimor111/shopping-cart-purescript.svg?branch=master)](https://travis-ci.com/Nimor111/shopping-cart-purescript)

Shopping cart application from PFP with Scala book written in Purescript

## Dev environment

### Setup dev env
The project uses `nix` and `direnv`, so just running `direnv allow` should work once those tools are installed (or you can just run `nix-shell` to get the dev env).
Also, run `yarn` for the yarn dependencies (these should be migrated to be managed by `node2nix`).

### Using the dev env

Spin up the database

``` sh
docker-compose up -d
```

Have two tabs open with the following commands:

For watching and rebuilding the code with `spago`

``` sh
yarn watch
```

For starting the actual `node` server

``` sh
./node_modules/.bin/nodemon dist/app.js
```

To format the code 

``` sh
purty --write src && purty --write test
```
