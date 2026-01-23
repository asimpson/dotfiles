{ pkgs, ... }:

{
  services.xserver = {
    dpi = 110;
    displayManager.sessionCommands = ''
      ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
      Xft.dpi: 110
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
    GDK_SCALE = "1.15";
    GDK_DPI_SCALE = "1.15";
    QT_SCALE_FACTOR = "1.15";
  };
}
