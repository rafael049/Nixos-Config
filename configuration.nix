# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, fonts, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # Windows Manager
      ./wm/xmonad.nix
      ./wm/mate.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.supportedFilesystems = [ "ntfs" ];


  networking.hostName = "nixos"; # Define your hostname.

  networking.useDHCP = false;
  networking.interfaces.enp7s0.useDHCP = true;
  networking.interfaces.wlp6s0.useDHCP = true;
  

  # bloqueando sites
  networking.extraHosts = ''
                       #127.0.0.1  www.youtube.com
  '';

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";


  # Enable wpa_supplicant
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "br-abnt2.map";
  };


  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # AMD driver
  boot.initrd.kernelModules = ["amdgpu" ];

  # Enable Opengl
  hardware.opengl.enable = true;

  # Use xonsh as shell
  programs.xonsh.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.rafael049 = {
    isNormalUser = true;
    extraGroups = [ "wheel" "NetworkingManager" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.xonsh;
  };


  # List packages installed in system profile. To search, run:
  # $ nix search wget

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
     vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
     wget
     curl
     brave
     mpv
     alacritty
     rofi
     xterm
     emacs
     git
     mpd
     zip
     unzip
   ];

   fonts.fonts = with pkgs; [
     noto-fonts
     noto-fonts-cjk
     noto-fonts-emoji
     liberation_ttf
     proggyfonts
     iosevka
     roboto
     nerdfonts
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
  system.stateVersion = "21.11"; # Did you read the comment?

}

