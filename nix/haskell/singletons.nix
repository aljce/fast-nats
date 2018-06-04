{ mkDerivation, base, Cabal, containers, directory, filepath, mtl
, process, stdenv, syb, tasty, tasty-golden, template-haskell, text
, th-desugar
}:
mkDerivation {
  pname = "singletons";
  version = "2.4.1";
  sha256 = "1kzrl9njvkbvxylk9jg61vy3ksmxmzymci5hdp0ilpsah4620yjx";
  libraryHaskellDepends = [
    base containers mtl syb template-haskell text th-desugar
  ];
  testHaskellDepends = [
    base Cabal directory filepath process tasty tasty-golden
  ];
  homepage = "http://www.github.com/goldfirere/singletons";
  description = "A framework for generating singleton types";
  license = stdenv.lib.licenses.bsd3;
}
