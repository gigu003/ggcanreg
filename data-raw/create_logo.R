## code to prepare `create_logo` dataset goes here
library(hexSticker)
library(png)
map <- grid::rasterGrob(
  readPNG("data-raw/logo_ggcanreg.png"),
  interpolate = TRUE
)

sticker(
  map,
  s_x = 1,
  s_y = 1.2,
  s_width = 0.9,
  s_height = 0.9,
  package = "ggcanreg",
  p_y = 0.70,
  p_size = 45,
  p_color = "#393e46",
  p_family = "sans",
  p_fontface = "bold",
  h_size = 0.3,
  h_color = "#393e46",
  h_fill = "#c1d1cf",
  filename = "data-raw/logo.png",
  asp = 1,
  dpi = 600
)


use_logo("logo.png")

file.remove("logo.png")
