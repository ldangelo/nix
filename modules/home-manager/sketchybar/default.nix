{pkgs,  config, ...}: {
   xdg.file = {
     ".lcal/share/sketchybar_lua/sketchybar_lua.so" = pkgs.sketchybar-lua; 
   }; 
     
   xdg.configFile = {
     "sketchybar" = {
       source = builtins.path {
         path = ./sketchybar;
       };
     };
   }; 
}
