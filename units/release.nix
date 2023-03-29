let
 pkgs = import <nixpkgs> { };
in
{
  units = pkgs.haskellPackages.callPackage ./default.nix { };
}
 
