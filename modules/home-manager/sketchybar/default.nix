{config, ...}: {
   xdg.configFile = {
     "sketchybar" = {
       source = builtins.path {
         path = ./sketchybar;
       };
     };
   }; 
}
