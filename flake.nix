{
  description = "meguri: 購入調査の決まった作業を受け持つ CLI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        {
          self',
          pkgs,
          lib,
          ...
        }:
        {
          haskellProjects.default = {
            # ビルドに要るファイルだけを見る（docs などの変更で作り直さない）
            projectRoot = builtins.toString (
              lib.fileset.toSource {
                root = ./.;
                fileset = lib.fileset.unions [
                  ./app
                  ./src
                  ./test
                  ./meguri.cabal
                ];
              }
            );
            devShell.tools = hp: { inherit (hp) cabal-install haskell-language-server; };
          };

          packages.default = pkgs.haskell.lib.justStaticExecutables self'.packages.meguri;
          apps.default = {
            type = "app";
            program = "${self'.packages.default}/bin/meguri";
          };

          # テストは、パッケージのビルドの中で走る
          checks.tests = self'.packages.meguri;

          checks.channels-md =
            pkgs.runCommand "meguri-channels-md-fresh" { nativeBuildInputs = [ self'.packages.default ]; }
              ''
                meguri sites --format md > generated.md
                diff -u ${./docs/channels.md} generated.md \
                  || { echo "docs/channels.md が古い。meguri sites --format md > docs/channels.md で作り直す"; exit 1; }
                touch $out
              '';
        };
    };
}
