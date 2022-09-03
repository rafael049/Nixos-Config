{Config, lib, pkgs, ...}:

{
  services = {
    upower.enable = true;
    
    xserver = {
      enable = true;
      layout = "br";
      videoDrivers = [ "amdgpu" ];
      deviceSection = ''
        Option "TearFree" "true"
      '';

      displayManager.defaultSession = "none+xmonad";
      
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      xkbOptions = "caps:escape";
    };
  };
}
