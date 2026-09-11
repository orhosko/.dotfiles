{
  description = "Declarative Nix config on Fedora";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
      };

      # Comment out a feature import to omit its packages and TeX requirements.
      features = [
        (import ./emacs.nix { inherit pkgs; })
        (import ./lsp.nix { inherit pkgs; })
        (import ./tex.nix { inherit pkgs; })
      ];

      tex = pkgs.texliveBasic.withPackages (
        ps: pkgs.lib.concatMap (feature: (feature.texPackages or (_: [ ])) ps) features
      );
    in
    {
      packages.${system}.default = pkgs.buildEnv {
        name = "fedora-nix-env";
        paths =
          pkgs.lib.concatMap (feature: feature.paths) features
          ++ [ tex ]
          ++ [
            # langs
            pkgs.go
            pkgs.gopls
            pkgs.gore

            pkgs.rustup

            pkgs.sbcl
            pkgs.zig
            pkgs.ghc

            pkgs.ocaml
            pkgs.opam
            pkgs.dune
            pkgs.ocamlPackages.utop
            pkgs.ocamlPackages.merlin

            pkgs.clang-tools

            pkgs.uv

            # tools
            pkgs.vscode
            pkgs.neovide
            # pkgs.opencode

            # wm utils
            pkgs.bluetui
            pkgs.ncdu # disk usage analyzer
            pkgs.pamixer
            pkgs.flameshot
            # pkgs.sshfs # i dont know why but it is not connecting
            # pgks.swaylock
            # pkgs.swaybg
            # pkgs.swayidle
            # pkgs.wl-clipboard
            # pkgs.rofi

            # fonts
            pkgs.iosevka
            pkgs.jetbrains-mono

            # terminal
            # pkgs.ghostty

            # rest
            pkgs.google-chrome
            pkgs.anki
            pkgs.keepassxc
            pkgs.proton-vpn

            # media
            pkgs.mpv
            pkgs.foliate
            pkgs.czkawka-full
            pkgs.vlc

            # dev
            pkgs.postgresql
            pkgs.sqlite
            pkgs.inetutils # for telnet
            pkgs.mold

            # misc
            pkgs.stress-ng
            pkgs.rlwrap
            pkgs.patch
            pkgs.cloc
            pkgs.normcap
            pkgs.tealdeer
            pkgs.gnucash
            pkgs.woomer

            # utils
            pkgs.fd
            pkgs.ripgrep
            pkgs.pandoc

            # doom-emacs
            ## python
            pkgs.python3Packages.isort
            pkgs.pipenv
            pkgs.python3Packages.pytest

            ## web
            pkgs.html-tidy
            pkgs.stylelint
            pkgs.js-beautify
          ];
      };
    };
}
