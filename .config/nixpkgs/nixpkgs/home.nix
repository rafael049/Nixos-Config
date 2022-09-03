{ config, pkgs, environment, ... }:

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


  imports = [
    ./programs/bash/default.nix
    ./programs/xmonad/default.nix
    ./programs/rofi/default.nix
    #./programs/ranger/default.nix
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
    discord
    gimp
    krita
    qbittorrent
    okular
    ardour
    tenacity
    shotcut
    translate-shell
    texlive.combined.scheme-full
    ranger
    hunspell
    sxiv
    rclone
    blender
    maim
    xclip
    xorg.xkill
    unrar
    bear
    nnn
    mpd
    playerctl
 
    elmPackages.elm
    pythonEnv
    gcc
    ccls
    rustup
    rust-analyzer
    mesa
    glew
    glfw
    cmake

    fsharp
  ];
}
