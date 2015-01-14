{ cabal, HaXml, network, networkUri, time }:

cabal.mkDerivation (self: {
  pname = "rss";
  version = "HEAD";
  src = ./.;
  buildDepends = [ HaXml network networkUri time ];
  meta = {
    homepage = "https://github.com/basvandijk/rss";
    description = "A library for generating RSS 2.0 feeds.";
    license = self.stdenv.lib.licenses.publicDomain;
    platforms = self.ghc.meta.platforms;
  };
})
