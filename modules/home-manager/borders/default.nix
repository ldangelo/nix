{config, pkgs, ...}: {
  # write .bordersrc file
  xdg.configFile = {
    "borders" = {
      
    source = builtins.path {
     name = "bordersrc";
     path = ./bordersrc;
    };
    };
  }; 
}
