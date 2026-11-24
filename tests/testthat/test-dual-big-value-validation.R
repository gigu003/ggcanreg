dual_validation_data <- data.frame(
  cancer = rep(c("肺癌", "胃癌", "肝癌"), 2),
  indicator = rep(c("发病率", "死亡率"), each = 3),
  rate = c(45.2, 22.5, 28.7, 38.1, 15.4, 20.2)
)

test_that("gg_dual_barchart validates data and preserves its alias", {
  p <- gg_dual_barchart(
    dual_validation_data,
    cancer,
    rate,
    indicator,
    legend_labels = c("发病率", "死亡率")
  )
  p_alias <- gg_dul_barchart(
    dual_validation_data,
    cancer,
    rate,
    indicator,
    legend_labels = c("发病率", "死亡率"),
    font_family = "serif",
    base_size = 11
  )

  expect_s3_class(p, "patchwork")
  expect_s3_class(p_alias, "patchwork")
  expect_identical(p$patches$annotation$theme$text$family, "sans")
  expect_equal(p$patches$annotation$theme$text$size, 12)
  expect_equal(p$patches$layout$widths, c(1, 1))
  expect_identical(p_alias$patches$annotation$theme$text$family, "serif")
  expect_equal(p_alias$patches$annotation$theme$text$size, 11)

  p_transparent <- gg_dual_barchart(
    dual_validation_data,
    cancer,
    rate,
    indicator,
    bg_color = "transparent"
  )
  expect_identical(
    p_transparent$patches$annotation$theme$plot.background$fill,
    "transparent"
  )
  expect_identical(
    p_transparent$patches$plots[[1]]$theme$panel.background$fill,
    "transparent"
  )
  expect_identical(
    p_transparent$theme$panel.background$fill,
    "transparent"
  )
  expect_error(gg_dual_barchart(1, cancer, rate, indicator), "data frame")
  expect_error(
    gg_dul_barchart(dual_validation_data, missing, rate, indicator),
    "missing"
  )
})

test_that("gg_dual_barchart aligns right labels and hides vertical axes", {
  p <- gg_dual_barchart(
    dual_validation_data,
    cancer,
    rate,
    indicator,
    legend_labels = c("发病率", "死亡率")
  )
  left <- p$patches$plots[[1]]
  right <- p

  expect_equal(right$theme$axis.text.y.right$hjust, 0)
  expect_equal(
    as.numeric(right$theme$axis.text.y.right$margin),
    c(0, 0, 0, 4)
  )
  for (axis_element in c(
    "axis.line.y",
    "axis.line.y.left",
    "axis.line.y.right",
    "axis.ticks.y",
    "axis.ticks.y.left",
    "axis.ticks.y.right"
  )) {
    expect_s3_class(left$theme[[axis_element]], "element_blank")
    expect_s3_class(right$theme[[axis_element]], "element_blank")
  }
})

test_that("gg_dual_barchart uses a shared default value-axis limit", {
  data <- data.frame(
    cancer = rep(c("Lung", "Breast", "Stomach"), 2),
    indicator = rep(c("Incidence", "Mortality"), each = 3),
    rate = c(120, 80, 60, 45, 30, 20)
  )
  p <- gg_dual_barchart(data, cancer, rate, indicator)
  left <- p$patches$plots[[1]]
  right <- p
  left_limits <- left$scales$get_scales("x")$limits
  right_limits <- right$scales$get_scales("x")$limits

  expect_equal(abs(left_limits[1]), right_limits[2])
  expect_equal(left_limits[2], right_limits[1])
})

test_that("gg_dual_barchart validates values and display arguments", {
  missing_data <- dual_validation_data
  missing_data$rate[1] <- NA_real_
  missing_group <- dual_validation_data
  missing_group$indicator[1] <- NA_character_

  expect_error(
    gg_dual_barchart(missing_data, cancer, rate, indicator),
    "finite, non-missing"
  )
  expect_error(
    gg_dual_barchart(missing_group, cancer, rate, indicator),
    "two non-missing"
  )
  expect_error(
    gg_dual_barchart(
      dual_validation_data,
      cancer,
      rate,
      indicator,
      top_n = 1.5
    ),
    "positive integer"
  )
  expect_error(
    gg_dual_barchart(
      dual_validation_data,
      cancer,
      rate,
      indicator,
      legend_labels = "指标"
    ),
    "legend_labels"
  )
  expect_error(
    gg_dual_barchart(
      dual_validation_data,
      cancer,
      rate,
      indicator,
      base_size = 0
    ),
    "greater than 0"
  )
})

test_that("gg_big_value validates scalar card inputs", {
  p <- gg_big_value(45.2, "发病率", value_digits = 1, value_suffix = "/10万")

  expect_s3_class(p, "ggplot")
  expect_error(gg_big_value(c(1, 2), "发病率"), "value")
  expect_error(gg_big_value(1, c("发病率", "死亡率")), "text")
  expect_error(gg_big_value(1, "发病率", title_case = NA), "title_case")
  expect_error(gg_big_value(1, "发病率", value_digits = -1), "value_digits")
  expect_error(gg_big_value(1, "发病率", value_size = 0), "value_size")
  expect_error(
    gg_big_value(1, "发病率", value_fun = function(x) c(x, x)),
    "must return"
  )
})
