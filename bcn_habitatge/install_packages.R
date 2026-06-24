# install_packages.R — Instal·la tots els paquets necessaris
pkgs <- c(
  "shiny", "shinydashboard", "shinyWidgets",
  "ggplot2", "dplyr", "tidyr", "scales",
  "ggrepel", "leaflet", "DT",
  "readr", "purrr", "stringr"
)
install.packages(pkgs[!pkgs %in% installed.packages()[, "Package"]])
