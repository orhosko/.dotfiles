{ pkgs }:
let
  tex = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-basic

      # for preview and export as html
      dvisvgm 
      dvipng 

      wrapfig
      amsmath
      amsfonts
      ulem
      hyperref
      capt-of

      metafont
      collection-fontsrecommended
      bookmark
      cm-super

      # org-latex-src-block-backend
      listings
      xcolor;
  };

  vanillaEmacsWrapper = pkgs.writeShellScriptBin "vanilla-emacs" ''
    exec emacs --init-directory="$HOME/.config/emacs-vanilla/"
  '';

  vanillaEmacsDesktop = pkgs.makeDesktopItem {
    name = "vanilla-emacs";
    desktopName = "Vanilla Emacs";
    comment = "Emacs with custom init directory";
    exec = "${vanillaEmacsWrapper}/bin/vanilla-emacs";
    icon = "emacs";
    categories = [ "Development" "TextEditor" ];
  };
in {
  paths = [
    tex
    vanillaEmacsWrapper
    vanillaEmacsDesktop
  ];
}
