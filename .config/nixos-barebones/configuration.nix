# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, unstablePkgs, lib, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
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
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  programs.hyprland.enable = true;
  programs.niri.enable = true;

  programs.nautilus-open-any-terminal = {
    enable = true;
    terminal = "alacritty";
  };

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

  services.tailscale.enable = true;

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
  documentation.dev.enable = true;

  hardware.facetimehd.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [

    man-pages
    man-pages-posix

    btop
    fastfetch

    unstablePkgs.neovim
    unstablePkgs.emacs

    anki-bin
    mpv

    fd

    ripgrep
    unzip

    stow

    openfortivpn

    jq
    socat

    git
    gh

    libreoffice-fresh
    hunspell
    hunspellDicts.tr_TR

    syncthing

    alacritty

    tmux

    killall

    wev

    hyprshot
    hyprpaper

    rofi
    waybar
    brightnessctl
    playerctl
    dunst
    mako
    libnotify
    pavucontrol
    swaybg

    tldr

    normcap
    foliate
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
  services.openssh.enable = true;

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
  system.stateVersion = "24.11"; # Did you read the comment?

  # services.flatpak.enable = true;

  environment.variables.EDITOR = "nvim";

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  services.desktopManager.gnome = {
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

  fonts.packages = with pkgs; [
    jetbrains-mono
    inter
  ];

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

  environment.sessionVariables = {
    # LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib";
    GSK_RENDERER = "opengl"; # vulkan renderer causes problems
  };

  environment.gnome.excludePackages = with pkgs; [ 
    file-roller
  ];

  nixpkgs.config.permittedInsecurePackages = [
    "broadcom-sta-6.30.223.271-57-6.12.40"
    "broadcom-sta-6.30.223.271-57-6.12.50"
    "broadcom-sta-6.30.223.271-57-6.12.51"
    "broadcom-sta-6.30.223.271-59-6.12.61"
    "broadcom-sta-6.30.223.271-59-6.18.46"
  ];
}
