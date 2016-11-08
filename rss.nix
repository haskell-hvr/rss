{ mkDerivation, base, HaXml, network, network-uri, old-locale
, stdenv, time
}:
mkDerivation {
  pname = "rss";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [
    base HaXml network network-uri old-locale time
  ];
  homepage = "https://github.com/basvandijk/rss";
  description = "A library for generating RSS 2.0 feeds.";
  license = stdenv.lib.licenses.publicDomain;
}
