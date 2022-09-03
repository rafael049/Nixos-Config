{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rafael049";
  home.homeDirectory = "/home/rafael049";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;


  imports = [
    ./programs/xmonad/default.nix
    ./programs/rofi/default.nix
    ./programs/alacritty/default.nix
    ./services/emacs/default.nix
    ./services/polybar/default.nix
    ./services/dunst/default.nix
    ./services/udiskie/default.nix
    ./services/picom/default.nix
  ];

  nixpkgs.config = {
    allowUnfree = true;
  };



  home.packages = with pkgs; let
    pythonEnv = pkgs.python39.withPackages (p: with p; [
      numpy
      matplotlib
      scikitimage
      pandas
      ipython
      jedi-language-server
    ]);
  in [
    ### Applications ###
    # System
    ranger
    hunspell
    playerctl
    xclip
    xorg.xkill
    nnn
    mpd
    exa
    # Media
	  spotify
    discord
    gimp
    krita
    okular
    ardour
    tenacity
    shotcut
    sxiv
    blender
    youtube-dl
    pdftk
    imagemagick
    obs-studio
    # Games
    mupen64plus
    gzdoom
    eduke32
    godot
    steam
    # Network
    qbittorrent
    rclone
    tor-browser-bundle-bin
    # Util
    translate-shell
    texlive.combined.scheme-full
    maim
    unrar
    bear
    hunspell
    aspell
    aspellDicts.pt_BR
    libreoffice
    gucharmap
    ueberzug
    ffmpegthumbnailer
    # Development
    elmPackages.elm
    pythonEnv
    gcc
    ccls
    cmake
    lua

    ### Development Libraries ###
    # Opengl
    mesa
    glew
    glfw
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
  ];
}
