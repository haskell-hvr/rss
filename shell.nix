let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      rss = self.callPackage ./rss.nix {};
    };
  };
in pkgs.myEnvFun {
     name = haskellPackages.rss.name;
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.rss.propagatedNativeBuildInputs)))
     ];
   }
