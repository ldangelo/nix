{pkgs,  config, ...}: {
   xdg.file = {
     ".local/share/sketcybar_lua/sketchybar_lua.so" = pkgs.sketcybar_lua; 
   }; 
     
   xdg.configFile = {
     "sketchybar" = {
       source = builtins.path {
         path = ./sketchybar;
       };
     };
   }; 
}
