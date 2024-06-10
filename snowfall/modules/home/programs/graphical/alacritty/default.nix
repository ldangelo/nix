{
  options,
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let cfg = config.${namespace}.programs.graphical.alacritty;
in {
  options.${namespace}.programs.graphical.alacritty = with types; {
    enable = mkBoolOpt false "Whether or not to enable alacritty.";
  };

  config = mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
      settings = {
        font = {
#          size = 14;
#          normal.family = "DejaVuSansM";
#          normal.style = "Regular";
        };
#        import = "themes/darcula.toml";
      };
    };
#    home.packages = with pkgs; [
#      alacritty
#    ];
  };
}
