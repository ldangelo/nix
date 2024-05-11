{config, pkgs, ...}: {
  # write .bordersrc file
  xdg.configFile = {
    name = "borders";
    source = builtins.path {
     name = "bordersrc";
     path = ./bordersrc;
    };
  }; 
}
