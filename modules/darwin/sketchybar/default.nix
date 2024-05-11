{home-manager, ...}: {
   home-manager.xdg.configFile = {
     "sketchybar" = {
       source = builtins.path {
         path = ./sketchybar;
       };
     };
   }; 
}
