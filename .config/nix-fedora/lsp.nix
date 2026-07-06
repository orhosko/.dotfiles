{ pkgs }:
{
  paths = [
    pkgs.lua-language-server

    pkgs.typescript-language-server

    pkgs.verible

    pkgs.cmake-language-server

    # pkgs.python314Packages.python-lsp-server

    pkgs.shellcheck

    pkgs.clj-kondo

    pkgs.gomodifytags
    pkgs.gotests

    pkgs.haskell-language-server
    pkgs.haskellPackages.hoogle
    pkgs.cabal-install

    pkgs.ocamlPackages.ocaml-lsp
    pkgs.ocamlPackages.ocamlformat
    pkgs.ocamlPackages.ocp-indent

    pkgs.nixfmt
  ];
}
