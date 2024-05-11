{...}: {
  # write .bordersrc file
  xdg.configjfile = {
    name = "borders";
    source = builtins.path {
     name = "bordersrc";
     path = ./bordersrc;
    };
  }; 
}
