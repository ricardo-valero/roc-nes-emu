{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # Pinned rev = roc nightly-2026-08-10-7df8509; keep in sync with
    # roc-web, roc-ngb-emu, and roc-ray's .roc-version.
    roc-overlay.url = "github:roc-lang/roc-overlay/948651d6d4c10e33b519bb30ff3900eb16a3dc3a";
    roc-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = {
    self,
    nixpkgs,
    roc-overlay,
    ...
  }: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
  in {
    devShells = nixpkgs.lib.genAttrs systems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      roc-pkgs = roc-overlay.packages.${system};
    in {
      default = pkgs.mkShell {
        buildInputs = builtins.attrValues {
          inherit (pkgs) nixd agent-browser;
          inherit (roc-pkgs) nightly;
        };
      };
    });
  };
}
