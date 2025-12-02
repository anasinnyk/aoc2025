{
  pkgs,
  ...
}:

{
  packages = with pkgs.haskellPackages; [
    hoogle
    doctest
    haskell-dap
    Cabal_3_16_0_0
    haskell-language-server
  ];

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc912;
  };
}
