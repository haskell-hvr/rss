let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./rss.nix {}
