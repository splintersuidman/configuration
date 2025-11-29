{ pkgs, config, ... }: {
  gtk = {
    enable = false;
    theme = {
      name = "Adwaita-dark";
    };
    font = {
      name = "DejaVu Sans 10";
    }; 
  };
}
