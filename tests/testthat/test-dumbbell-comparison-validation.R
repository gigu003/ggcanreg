dumbbell_validation_data <- data.frame(
  cancer = rep(c("肺癌", "胃癌", "肝癌"), each = 2),
  sex = rep(c("男", "女"), 3),
  rate = c(50.1, 28.2, 22.5, 13.1, 30.2, 11.5)
)

comparison_validation_data <- data.frame(
  registry = c("登记处甲", "登记处乙", "登记处丙"),
  incidence = c(42.1, 38.7, 35.2)
)

test_that("gg_dumbbell validates its core data contract", {
  p <- gg_dumbbell(
    dumbbell_validation_data,
    cancer,
    rate,
    sex,
    group_order = c("男", "女")
  )

  expect_s3_class(p, "ggplot")
  expect_error(gg_dumbbell(1, cancer, rate, sex), "data frame")
  expect_error(
    gg_dumbbell(dumbbell_validation_data, missing, rate, sex),
    "missing"
  )
  expect_error(
    gg_dumbbell(
      transform(dumbbell_validation_data, rate = as.character(rate)),
      cancer,
      rate,
      sex
    ),
    "numeric column"
  )
})

test_that("gg_dumbbell uses the widest difference label for every background", {
  data <- data.frame(
    cancer = rep(c("肺癌", "胃癌", "肝癌"), each = 2),
    sex = rep(c("男", "女"), 3),
    rate = c(100, 1, 20, 2, 3, 2)
  )

  p <- gg_dumbbell(
    data,
    cancer,
    rate,
    sex,
    group_order = c("男", "女"),
    sort = FALSE
  )

  expect_length(unique(p$layers[[4]]$data$diff_label_background), 1)
  expect_identical(
    p$layers[[4]]$data$diff_label_background,
    rep("99.0", 3)
  )
  expect_setequal(
    p$layers[[5]]$data$diff_label,
    c("99.0", "18.0", " 1.0")
  )
})

test_that("gg_dumbbell rejects incomplete and invalid scalar inputs", {
  missing_data <- dumbbell_validation_data
  missing_data$rate[1] <- NA_real_

  expect_error(
    gg_dumbbell(missing_data, cancer, rate, sex),
    "finite, non-missing"
  )
  expect_error(
    gg_dumbbell(dumbbell_validation_data, cancer, rate, sex, horizontal = NA),
    "horizontal"
  )
  expect_error(
    gg_dumbbell(dumbbell_validation_data, cancer, rate, sex, label_digits = -1),
    "label_digits"
  )
  expect_error(
    gg_dumbbell(dumbbell_validation_data, cancer, rate, sex, point_size = 0),
    "point_size"
  )
})

test_that("gg_comparison validates finite values and scalar arguments", {
  p <- gg_comparison(
    comparison_validation_data,
    incidence,
    registry,
    highlight = "登记处甲"
  )

  expect_s3_class(p, "ggplot")
  expect_error(
    gg_comparison(1, incidence, registry, highlight = "登记处甲"),
    "data frame"
  )

  missing_data <- comparison_validation_data
  missing_data$incidence[1] <- NA_real_
  expect_error(
    gg_comparison(missing_data, incidence, registry, highlight = "登记处甲"),
    "finite, non-missing"
  )
  expect_error(
    gg_comparison(
      comparison_validation_data,
      incidence,
      registry,
      highlight = "登记处甲",
      add_label = NA
    ),
    "add_label"
  )
  expect_error(
    gg_comparison(
      comparison_validation_data,
      incidence,
      registry,
      highlight = "登记处甲",
      label_digits = -1
    ),
    "label_digits"
  )
})

test_that("comparison and dumbbell share publication typography and backgrounds", {
  p_comparison <- gg_comparison(
    comparison_validation_data,
    incidence,
    registry,
    highlight = "登记处甲",
    font_family = "serif",
    base_size = 11,
    bg_color = "transparent"
  )
  p_dumbbell <- gg_dumbbell(
    dumbbell_validation_data,
    cancer,
    rate,
    sex,
    group_order = c("男", "女"),
    title = "癌种比较",
    x_lab = "率",
    font_family = "serif",
    base_size = 11,
    bg_color = "transparent"
  )

  expect_identical(p_comparison$theme$text$family, "serif")
  expect_equal(p_comparison$theme$text$size, 11)
  expect_identical(p_comparison$theme$plot.background$fill, "transparent")
  expect_identical(p_comparison$layers[[3]]$aes_params$family, "serif")

  p_comparison_multi <- gg_comparison(
    comparison_validation_data,
    incidence,
    registry,
    highlight = c("登记处甲", "登记处乙")
  )
  expect_equal(
    length(unique(
      p_comparison_multi$layers[[3]]$data$.ggcanreg_comparison_label_y
    )),
    2
  )

  expect_identical(p_dumbbell$theme$text$family, "serif")
  expect_equal(p_dumbbell$theme$text$size, 11)
  expect_identical(p_dumbbell$theme$panel.background$fill, "transparent")
  expect_identical(p_dumbbell$layers[[3]]$aes_params$family, "serif")
  expect_identical(p_dumbbell$layers[[4]]$aes_params$family, "serif")
  expect_identical(p_dumbbell$labels$title, "癌种比较")
  expect_identical(p_dumbbell$labels$x, "率")
  expect_gt(
    rlang::eval_tidy(
      p_dumbbell$layers[[4]]$mapping$x,
      data = p_dumbbell$layers[[4]]$data
    ),
    max(p_dumbbell$data$y_label)
  )
  expect_no_warning(ggplot2::ggplot_build(p_dumbbell))

  expect_error(
    gg_comparison(
      comparison_validation_data,
      incidence,
      registry,
      highlight = "登记处甲",
      bg_color = "not-a-color"
    ),
    "valid R color"
  )
  expect_error(
    gg_dumbbell(
      dumbbell_validation_data,
      cancer,
      rate,
      sex,
      bg_color = 1
    ),
    "single character"
  )
})
