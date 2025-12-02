{
  description = "fusion-plugin";

  inputs = {
    basepkgs.url = "github:composewell/nixpack/dedcd2cba7dc0715ace19d185d23dd1751daed6e";
    nixpkgs.url = "github:NixOS/nixpkgs/6c9a78c09ff4d6c21d0319114873508a6ec01655"; # nixos-unstable
    nixpkgs-darwin.url = "github:NixOS/nixpkgs/6c9a78c09ff4d6c21d0319114873508a6ec01655"; # nixos-unstable
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    basepkgs.nixpack.mkOutputs {
      inherit nixpkgs nixpkgs-darwin basepkgs;
      name = "fusion-plugin";
      sources = basepkgs.nixpack.lib.localSource "fusion-plugin" ./.;
      packages = basepkgs.nixpack.lib.devPackage "fusion-plugin";
    };
}
