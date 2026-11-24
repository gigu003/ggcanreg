test_that("gg_north_arrow creates a reusable annotation layer", {
  arrow <- gg_north_arrow()
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    arrow

  expect_s3_class(arrow, "Layer")
  expect_s3_class(arrow$geom_params$grob, "gTree")
  expect_length(arrow$geom_params$grob$children, 1L)
  expect_s3_class(arrow$geom_params$grob$children[[1]]$vp, "viewport")
  expect_s3_class(ggplot2::ggplot_build(plot), "ggplot_built")
})

test_that("gg_north_arrow supports every corner and custom dimensions", {
  arrows <- lapply(
    c("tl", "tr", "bl", "br"),
    \(location) {
      gg_north_arrow(
        location = location,
        width = 1.2,
        height = 1.6,
        pad_x = 0,
        pad_y = 0,
        line_width = 0,
        text_size = 10
      )
    }
  )

  expect_length(arrows, 4L)
  expect_identical(
    vapply(arrows, inherits, logical(1), what = "Layer"),
    rep(TRUE, 4L)
  )
})

test_that("gg_north_arrow validates its arguments", {
  expect_snapshot(error = TRUE, gg_north_arrow(location = "center"))
  expect_snapshot(error = TRUE, gg_north_arrow(width = 0))
  expect_snapshot(error = TRUE, gg_north_arrow(pad_x = -0.1))
  expect_snapshot(error = TRUE, gg_north_arrow(text_size = Inf))
})
