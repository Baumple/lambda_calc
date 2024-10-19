{
  inputs = {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, ... }: 
  flake-utils.lib.eachDefaultSystem(system: 
  let 
    overlays = [ (import rust-overlay) (final: prev: {
      gleam = prev.gleam.overrideAttrs(oldAttrs: rec {
        src = prev.fetchFromGitHub {
          owner = "gleam-lang";
          repo = "gleam";
          rev = "refs/tags/v1.5.1";
          hash = "sha256-4/NDZGq62M0tdWerIkmoYS0WHC06AV8c9vlo/6FhsAo=";
        };
        cargoDeps = oldAttrs.cargoDeps.overrideAttrs (prev.lib.const {
          name = "gleam-lang-vendor.tar.gz";
          inherit src;
          outputHash = "sha256-igAPbQVjecuOHnpPIKUgSR5LAD703glUQapDbj7Kdlg=";
        });
      });
    }) ];

    rust = pkgs.rust-bin.stable.latest.default.override {
      extensions = [
        "rust-src"
        "rust-analyzer"
      ];
    };
    pkgs = import nixpkgs {
      inherit system overlays;
    };
  in
  {
    devShells.default = with pkgs; mkShell {
      buildInputs = [
        rust
        gleam
        inotify-tools
        erlang_27
      ];
    };
  });
}
