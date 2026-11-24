## code to prepare `ggcanreg_color_scheme` dataset goes here
ggcanreg_color_schemes <- list(
  default = list(
    background = "#FFFFFF",
    title = "#333333",
    substitle = "#333333",
    axis_text = "#666666",
    axis_line = "#AAAAAA",
    legend_background = "#FFFFFF",
    legend_text = "#333333"
  )
)
usethis::use_data(ggcanreg_color_schemes, internal = TRUE, overwrite = TRUE)
