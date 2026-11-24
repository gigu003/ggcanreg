test_that("canreg_palette returns colors and validates palette names", {
  expect_type(canreg_palette("default"), "character")
  expect_length(canreg_palette("sex", n = 2), 2)
  expect_error(canreg_palette("not-a-palette"), "Unknown palette")
})

test_that("theme_canreg returns a ggplot theme", {
  expect_s3_class(theme_canreg(), "theme")
  expect_s3_class(theme_block(), "theme")
  expect_s3_class(theme_grid(), "theme")
  expect_s3_class(theme_canreg_map(), "theme")
})

test_that("theme_canreg provides report, publication, and slide styles", {
  report_theme <- theme_canreg(style = "report")
  publication_theme <- theme_canreg(style = "publication")
  slides_theme <- theme_canreg(style = "slides")

  expect_equal(report_theme$legend.position, "top")
  expect_equal(publication_theme$legend.position, "bottom")
  expect_equal(slides_theme$legend.position, "bottom")
  expect_equal(publication_theme$text$size, 8)
  expect_equal(slides_theme$text$size, 18)
  expect_s3_class(publication_theme$axis.line, "element_line")
  expect_s3_class(publication_theme$panel.grid.major, "element_blank")
  expect_s3_class(slides_theme$axis.ticks, "element_blank")
})

test_that("theme_canreg supports custom plot margins", {
  p_margin <- ggplot2::margin(t = 10, r = 20, b = 30, l = 40)
  th <- theme_canreg(margin = p_margin)

  expect_s3_class(th, "theme")
  expect_equal(as.numeric(th$plot.margin), c(10, 20, 30, 40))

  th_numeric <- theme_canreg(margin = c(5, 10, 5, 10))
  expect_equal(as.numeric(th_numeric$plot.margin), c(5, 10, 5, 10))

  th_single <- theme_canreg(margin = 6)
  expect_equal(as.numeric(th_single$plot.margin), c(6, 6, 6, 6))

  expect_error(
    theme_canreg(margin = c(1, 2, 3)),
    "numeric vector of length 4"
  )
})

test_that("theme shortcuts pass custom margins to theme_canreg", {
  p_margin <- ggplot2::margin(t = 8, r = 9, b = 10, l = 11)
  th <- theme_grid(margin = p_margin)

  expect_s3_class(th, "theme")
  expect_equal(as.numeric(th$plot.margin), c(8, 9, 10, 11))
})

test_that("theme_canreg pie style hides axes and facet strips", {
  th <- theme_canreg(style = "pie")

  expect_s3_class(th, "theme")
  expect_s3_class(th$axis.title, "element_blank")
  expect_s3_class(th$axis.title.x, "element_blank")
  expect_s3_class(th$axis.title.y, "element_blank")
  expect_s3_class(th$axis.text, "element_blank")
  expect_s3_class(th$axis.text.x, "element_blank")
  expect_s3_class(th$axis.text.y, "element_blank")
  expect_s3_class(th$axis.ticks, "element_blank")
  expect_s3_class(th$axis.ticks.x, "element_blank")
  expect_s3_class(th$axis.ticks.y, "element_blank")
  expect_s3_class(th$axis.line, "element_blank")
  expect_s3_class(th$axis.line.x, "element_blank")
  expect_s3_class(th$axis.line.y, "element_blank")
  expect_s3_class(th$panel.grid, "element_blank")
  expect_s3_class(th$panel.grid.major, "element_blank")
  expect_s3_class(th$panel.grid.major.x, "element_blank")
  expect_s3_class(th$panel.grid.major.y, "element_blank")
  expect_s3_class(th$panel.grid.minor, "element_blank")
  expect_s3_class(th$panel.grid.minor.x, "element_blank")
  expect_s3_class(th$panel.grid.minor.y, "element_blank")
  expect_s3_class(th$panel.border, "element_blank")
  expect_s3_class(th$strip.text, "element_blank")
  expect_s3_class(th$strip.background, "element_blank")
})

test_that("theme_canreg supports optional legend titles and axis text", {
  th_default <- theme_canreg()
  expect_s3_class(th_default$legend.title, "element_blank")

  th_legend <- theme_canreg(legend_title = TRUE)
  expect_s3_class(th_legend$legend.title, "element_text")

  th_axis <- theme_canreg(axis_text = FALSE)
  expect_s3_class(th_axis$axis.text, "element_blank")
  expect_s3_class(th_axis$axis.text.x, "element_blank")
})

test_that("theme_canreg validates common scalar arguments", {
  expect_error(theme_canreg(base_size = 0), "greater than 0")
  expect_error(theme_canreg(base_size = c(10, 12)), "single number")
  expect_error(theme_canreg(x_text_angle = NA_real_), "single number")
  expect_error(theme_canreg(legend_title = NA), "TRUE or FALSE")
  expect_error(theme_canreg(axis_title = NA), "TRUE or FALSE")
  expect_error(theme_canreg(axis_text = NA), "TRUE or FALSE")
  expect_error(theme_canreg(base_family = c("A", "B")), "single character")
})

test_that("theme_canreg_map hides map axes and grids", {
  th <- theme_canreg_map()

  expect_s3_class(th, "theme")
  expect_equal(th$legend.position, "right")
  expect_s3_class(th$axis.title, "element_blank")
  expect_s3_class(th$axis.text, "element_blank")
  expect_s3_class(th$axis.ticks, "element_blank")
  expect_s3_class(th$axis.line, "element_blank")
  expect_s3_class(th$panel.grid, "element_blank")
  expect_s3_class(th$panel.border, "element_blank")
})
