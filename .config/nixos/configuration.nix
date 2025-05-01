# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Istanbul";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "tr_TR.UTF-8";
    LC_IDENTIFICATION = "tr_TR.UTF-8";
    LC_MEASUREMENT = "tr_TR.UTF-8";
    LC_MONETARY = "tr_TR.UTF-8";
    LC_NAME = "tr_TR.UTF-8";
    LC_NUMERIC = "tr_TR.UTF-8";
    LC_PAPER = "tr_TR.UTF-8";
    LC_TELEPHONE = "tr_TR.UTF-8";
    LC_TIME = "tr_TR.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  programs.hyprland.enable = true;

  # Disable CUPS
  services.printing.enable = false;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.berkay = {
    isNormalUser = true;
    description = "berkay";
    extraGroups = [ "networkmanager" "wheel" "plugdev" ];
    packages = with pkgs; [
    #  thunderbird
    ];
  };

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  hardware.facetimehd.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  neovim
  tree-sitter
  wl-clipboard
  lua-language-server
  stylua

  anki-bin
  mpv

  google-chrome

  emacs
  fd

  ripgrep
  unzip

  stow

  git
  gh

  clang
  clang-tools
  gcc
  gdb
  llvmPackages_20.bintools-unwrapped

  zig
  zls

  verilator
  verible
  yosys
  nextpnr
  openfpgaloader
  python312Packages.apycula

  python3
  nodejs
  #go

  cmake
  gnumake
  ninja
  boost
  bazel

  # warp-terminal
  
  libreoffice-fresh
  hunspell
  hunspellDicts.tr_TR

  syncthing

  alacritty
  ghostty
  zellij
  minicom
  killall

  # lazygit

  wev

  rofi-wayland
  hyprshot
  waybar
  brightnessctl
  playerctl
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

  services.flatpak.enable = true;
  environment.variables.EDITOR = "nvim";
  # services.neovim.defaultEditor = true;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  services.xserver.desktopManager.gnome = {
    extraGSettingsOverrides = ''
      # Change default background
      # [org.gnome.desktop.background]
      # picture-uri='file://${pkgs.nixos-artwork.wallpapers.mosaic-blue.gnomeFilePath}'

      # Favorite apps in gnome-shell
      # [org.gnome.shell]
      # favorite-apps=['org.gnome.Console.desktop', 'org.gnome.Nautilus.desktop']
      
      [org.gnome.desktop.wm.preferences]
      focus-mode='sloppy'
    '';

    extraGSettingsOverridePackages = [
      pkgs.gsettings-desktop-schemas # for org.gnome.desktop
      # pkgs.gnome.gnome-shell # for org.gnome.shell
    ];
  };

services.emacs = {
  enable = true;
  package = pkgs.emacs;
};

fonts.packages = with pkgs; [
  jetbrains-mono
  inter
];

services.syncthing = {
  dataDir = "/home/berkay";
  enable = true;
  user = "berkay";
};

# services.syncthing = {
#   enable = true;
#   openDefaultPorts = true;
#   # user = "berkay";
#   settings.gui = {
#     user = "test";
#     password = "test1";
#   };
#   settings = {
#     devices = {
#       "Oppo A72" = { id = "RNKR4XS-LASRHNE-I72ZDRJ-732LURJ-XOL3QX6-FAOLJWV-VWRYINY-L2Q5XQV"; };
#     };
#     # folders = {
#     #   "sync" = {
#     #     path = "/home/berkay/Documents/sync";
#     #     devices = [ "Oppo A72" ];
#     #   };
#     #   "org" = {
#     #     path = "/home/berkay/Documents/org";
#     #     devices = [ "Oppo A72" ];
#     #   };
#     # };
#   };
# };

services.keyd = {
  enable = true;
  keyboards = {
    default = {
      ids = ["*"];
      settings = {
        main = {
          capslock = "overload(control, esc)";
          esc = "capslock";
	  control = "layer(nav)";
        };
        nav = {
	  j = "down";
	  k = "up";
	  h = "left";
	  l = "right";
	  p = "print";
	  n = "C-A-left";
	  shift-n = "C-A-S-left";
	  m = "C-A-right";
	  shift-m = "C-A-S-right";
        };
      };
    };
  };
};

swapDevices = [{
  device = "/swapfile";
  size = 6 * 1024; # 6GB
}];

nix.gc = {
  automatic = true;
  options = "--delete-older-than 7d";
};

nix.optimise.automatic = true;
nix.settings.auto-optimise-store = true;

programs.nix-ld.enable = true;
programs.nix-ld.libraries = with pkgs; [
  # Add any missing dynamic libraries for unpackaged
  # programs here, NOT in environment.systemPackages
];

environment = {
    sessionVariables = {
      LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib";
    };
  };

environment.gnome.excludePackages = with pkgs; [ 
	file-roller
	];

home-manager.users.berkay = { pkgs, ... }: {
    home.username = "berkay";
    home.homeDirectory = "/home/berkay";
    home.stateVersion = "25.05";

    programs.home-manager.enable = true;

    programs.zsh = {
        enable = true;
        enableCompletion = true;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;
    
        shellAliases = {
          ll = "ls -l";
          edit = "sudo -e";
          update = "sudo nixos-rebuild switch --flake ~/.dotfiles/.config/nixos --impure";
        };
    
        history.size = 10000;
        history.ignoreAllDups = true;
        history.path = "$HOME/.zsh_history";
        history.ignorePatterns = ["rm *" "pkill *" "cp *"];

        oh-my-zsh = {
        enable = true;
        plugins = ["git"];
        theme = "robbyrussell";
        };
    };

    home.file.".zellij.zshrc".source = /home/berkay/.zellij.zshrc;

};

programs.zsh.enable = true;
home-manager.useGlobalPkgs = true;
users.users.berkay.shell = pkgs.zsh;

services.udev.packages = [
      (pkgs.writeTextFile {
        name = "99-openfpgaloader";
        text = ''
# Copy this file to /etc/udev/rules.d/

ACTION!="add|change", GOTO="openfpgaloader_rules_end"

# gpiochip subsystem
SUBSYSTEM=="gpio", MODE="0664", GROUP="plugdev", TAG+="uaccess"

SUBSYSTEM!="usb|tty|hidraw", GOTO="openfpgaloader_rules_end"

# Original FT232/FT245 VID:PID
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6001", MODE="664", GROUP="plugdev", TAG+="uaccess"

# Original FT2232 VID:PID
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="664", GROUP="plugdev", TAG+="uaccess"

# Original FT4232 VID:PID
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6011", MODE="664", GROUP="plugdev", TAG+="uaccess"

# Original FT232H VID:PID
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6014", MODE="664", GROUP="plugdev", TAG+="uaccess"

# Original FT231X VID:PID
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6015", MODE="664", GROUP="plugdev", TAG+="uaccess"

# anlogic cable
ATTRS{idVendor}=="0547", ATTRS{idProduct}=="1002", MODE="664", GROUP="plugdev", TAG+="uaccess"

# altera usb-blaster
ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6001", MODE="664", GROUP="plugdev", TAG+="uaccess"
ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6002", MODE="664", GROUP="plugdev", TAG+="uaccess"
ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6003", MODE="664", GROUP="plugdev", TAG+="uaccess"

# altera usb-blasterII - uninitialized
ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6810", MODE="664", GROUP="plugdev", TAG+="uaccess"
# altera usb-blasterII - initialized
ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6010", MODE="664", GROUP="plugdev", TAG+="uaccess"

# dirtyJTAG
ATTRS{idVendor}=="1209", ATTRS{idProduct}=="c0ca", MODE="664", GROUP="plugdev", TAG+="uaccess"

# Jlink
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0105", MODE="664", GROUP="plugdev", TAG+="uaccess"

# NXP LPC-Link2
ATTRS{idVendor}=="1fc9", ATTRS{idProduct}=="0090", MODE="664", GROUP="plugdev", TAG+="uaccess"

# NXP ARM mbed
ATTRS{idVendor}=="0d28", ATTRS{idProduct}=="0204", MODE="664", GROUP="plugdev", TAG+="uaccess"

# icebreaker bitsy
ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="6146", MODE="664", GROUP="plugdev", TAG+="uaccess"

# numato systems
ATTRS{idVendor}=="2a19", ATTRS{idProduct}=="1009", MODE="644", GROUP="plugdev", TAG+="uaccess"

# orbtrace-mini dfu
ATTRS{idVendor}=="1209", ATTRS{idProduct}=="3442", MODE="664", GROUP="plugdev", TAG+="uaccess"

# QinHeng Electronics USB To UART+JTAG (ch347)
ATTRS{idVendor}=="1a86", ATTRS{idProduct}=="55dd", MODE="664", GROUP="plugdev", TAG+="uaccess"

LABEL="openfpgaloader_rules_end"
	'';
        destination = "/etc/udev/rules.d/99-openfpgaloader.rules";
      })
      (pkgs.writeTextFile {
        name = "99-ftdi";
        text = ''
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="0666"
	'';
        destination = "/etc/udev/rules.d/99-ftdi.rules";
      })
    ];
}
