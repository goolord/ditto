# Changelog

## 0.5.0 — 2026-08-27

### Changed
- Prefer GHC 9.10+ (`GHC2024`) with automatic fallback to `GHC2021` / `Haskell2010` on older GHC
- Widen dependency bounds (`base >= 4.14`, `text >= 1.2`) and drop the unused `containers` dependency
- Enable a stricter warning set on GHC 8.10+ (`-Wcompat`, unused packages, missing deriving strategies, …)
- Replace per-module `LANGUAGE` pragmas with cabal `default-extensions`
- Drop redundant constraints from public signatures (`prove`, `inputList`, `inputMaybe`, …); `inputMulti` does not require `FormInput` because decoding is caller-supplied
- Fix `inputFile` so `Default` environment renders an empty upload instead of an error (`Missing` on submit is unchanged)

### Added
- Export `hoistForm`
- Test suite covering decode, `Alternative`, `catchFormError`, `inputMulti`, `inputChoice`, and `inputFile` (including `Missing` submit path)
- GitHub Actions CI: Cabal matrix (GHC 9.6–9.14) and `nix flake check`
- `cabal.project`
- Nix flake (`flake.nix`, `flake.lock`) with `packages`, `checks`, and `devShells` outputs

### Removed
- `Setup.hs` (unused with `build-type: Simple`)

### Packaging
- Nix is flake-only; legacy `default.nix` and `shell.nix` removed
- nixpkgs pinned by rev in `flake.nix` / `flake.lock`
