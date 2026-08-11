{ pkgs, ... }:

let
  # 32" 4K (~138 PPI)
  dpi = 150;
in

{
  services.xserver = {
    dpi = dpi;
    # deviceSection = ''
    #   ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-1 --mode 3840x2160 --rate 120
    #   Option "VariableRefresh" "true"
    # '';
    displayManager.sessionCommands = ''
      ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
      Xft.dpi: ${toString dpi}
      Xft.autohint: 0
      Xft.lcdfilter: lcddefault
      Xft.hintstyle: hintslight
      Xft.hinting: 1
      Xft.antialias: 1
      Xft.rgba: rgb
      EOF
    '';
  };

  environment.variables = {
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "1.0";
    QT_SCALE_FACTOR = "1.0";
  };
}
