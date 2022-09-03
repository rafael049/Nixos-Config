
{
  programs.bash = {
    enable = true;
    # bash configuration
    bashrcExtra = ''
              export PATH=\"$PATH:/home/rafael049/.dotnet/tools\" 
    '';
  };
}
