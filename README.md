ditto [![Hackage](https://img.shields.io/hackage/v/ditto.svg)](https://hackage.haskell.org/package/ditto)
=========

![Image of ditto from pokemon](https://i.imgur.com/0i7qeeW.png)
> Its transformation ability is perfect. However, if it is made to laugh, it can't maintain its disguise.

A portable library which provides type-safe form generation and validation.

This core library is intended to be used in conjunction with other libraries such as [scotty-form](http://hackage.haskell.org/package/scotty-form) and [ditto-lucid](http://hackage.haskell.org/package/ditto-lucid).

## Requirements

Tested on GHC 9.6 through 9.14. GHC 9.10+ uses `GHC2024` by default; older supported GHC versions fall back to `GHC2021` or `Haskell2010` with the required extensions enabled in the cabal file.

### Cabal

Use this for day-to-day development (fast incremental rebuilds):

```
cabal test --enable-tests
```

### Nix (flake)

Pinned nixpkgs and package checks:

```
nix build
nix flake check
nix develop
```

- `nix flake check` — build library and run the test suite in an isolated Nix derivation (CI uses this).
- `nix develop` — enter a shell with GHC, `cabal-install`, and HLS; run `cabal test` for iterative work.
- Flake source includes all project files under the repo root (not only git-tracked paths). `flake.lock` pins nixpkgs.
