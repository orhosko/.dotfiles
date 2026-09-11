{ pkgs }:
{
  texPackages =
    ps: with ps; [
      scheme-basic # Minimal TeX Live base system.

      latex-bin # Provides core LaTeX commands, including pdflatex format support.
      pdftex # Provides the pdfTeX engine used by pdflatex.
      luatex # Provides LuaTeX/LuaLaTeX support.
      latexmk # Provides latexmk, a Perl script for automating LaTeX builds.

      # LaTeX packages:
      dvisvgm # Converts DVI/PDF/EPS output into SVG, useful for LaTeX previews.
      dvipng # Converts DVI output into PNG, useful for LaTeX previews.
      wrapfig # Allows text to wrap around figures and tables.
      amsmath # Provides AMS math environments like align, gather, and split.
      amsfonts # Provides AMS fonts, including blackboard-bold symbols like \mathbb.
      ulem # Provides underline and strikeout commands like \uline and \sout.
      hyperref # Adds clickable links, references, URLs, and PDF metadata.
      capt-of # Allows captions outside normal figure/table float environments.
      metafont # Provides the METAFONT engine for generating some TeX fonts.
      collection-fontsrecommended # Installs commonly recommended TeX fonts.
      bookmark # Improves PDF bookmark handling, usually with hyperref.
      cm-super # Provides Type 1 Computer Modern fonts for better PDF output.
      listings # Provides source-code listing environments.
      xcolor # Adds color support for text, tables, boxes, and listings.
      babel-turkish # Adds Turkish Babel language support and hyphenation.
      float # Adds stronger float placement control, especially [H].
      enumitem # Customizes itemize/enumerate/description list spacing and labels.
      sectsty # Customizes section heading fonts and styles.
      tocloft # Customizes table of contents/list of figures/list of tables formatting.
      caption # Customizes figure and table captions.
      algorithmicx # Provides pseudocode environments such as algpseudocode.
      algorithms # Provides the floating algorithm environment.
      makecell # Makes multiline table cells easier.
      pgf # Provides PGF/TikZ graphics support, including tikz.sty.
      ieeetran # Provides the IEEEtran document class for IEEE publications.
      comment # Provides block comments with the comment environment.
      changepage # Allows temporary margin/layout changes like adjustwidth.
      multirow # Provides the multirow command for table cells spanning multiple rows.
    ];
  paths = [
    pkgs.mermaid-cli # Provides mmdc, the Mermaid CLI renderer.
  ];
}
