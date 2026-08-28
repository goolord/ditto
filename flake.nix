{
  description = "ditto: type-safe HTML form generation and validation";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/9fbb54b33e91ee4ca368e35a78e0613c720600b3";

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      src = builtins.path {
        path = ./.;
        name = "ditto";
        filter = path: _type:
          let name = baseNameOf path;
          in !(builtins.elem name [
            ".git"
            "dist-newstyle"
            "result"
            "result-*"
            ".direnv"
            ".hie"
          ]);
      };
      mkPackages = pkgs:
        let
          library = pkgs.haskellPackages.callCabal2nix "ditto" src { };
          checked = pkgs.haskell.lib.overrideCabal library (_: { doCheck = true; });
        in
        { inherit library checked; };
    in
    {
      packages = forAllSystems (pkgs:
        let p = mkPackages pkgs;
        in {
          default = p.checked;
          ditto = p.checked;
        });

      checks = forAllSystems (pkgs: {
        default = (mkPackages pkgs).checked;
      });

      devShells = forAllSystems (pkgs:
        let
          hp = pkgs.haskellPackages;
          p = mkPackages pkgs;
        in {
          default = hp.shellFor {
            packages = _: [ p.library ];
            nativeBuildInputs = with hp; [
              cabal-install
              haskell-language-server
            ];
          };
        });
    };
}
