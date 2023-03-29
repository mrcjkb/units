{ mkDerivation, base, containers, deepseq, lens, lib, linear, mtl
, multimap, singletons, singletons-base, singletons-th, syb
, template-haskell, th-desugar, units-parser, vector-space
}:
mkDerivation {
  pname = "units";
  version = "2.4.1.5";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq lens linear mtl multimap singletons
    singletons-base singletons-th syb template-haskell th-desugar
    units-parser vector-space
  ];
  homepage = "https://github.com/goldfirere/units";
  description = "A domain-specific type system for dimensional analysis";
  license = lib.licenses.bsd3;
}
