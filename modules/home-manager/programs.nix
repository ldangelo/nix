{ pkgs, misc, ... }: {
    programs.direnv.enable = true; 
    programs.starship.enable = true;

  # User specified programs 
    programs.dircolors.enable = true;
}
