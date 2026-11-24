stack_validation_data <- data.frame(
  year = rep(2020:2021, each = 3),
  cancer = rep(c("肺癌", "胃癌", "肝癌"), 2),
  cases = c(45, 28, 31, 42, 25, 28)
)

heatmap_validation_data <- data.frame(
  registry = rep(c("登记处甲", "登记处乙"), each = 2),
  indicator = rep(c("MV", "DCO"), 2),
  sex = rep(c("男", "女"), 2),
  value = c(75.2, 2.1, 68.4, 3.5)
)

test_that("gg_stack validates mapped variables and numeric values", {
  p <- gg_stack(stack_validation_data, year, cases, fill = cancer)

  expect_s3_class(p, "ggplot")
  expect_error(
    gg_stack(stack_validation_data, missing, cases, fill = cancer),
    "missing"
  )
  expect_error(
    gg_stack(
      transform(stack_validation_data, cases = as.character(cases)),
      year,
      cases,
      fill = cancer
    ),
    "numeric column"
  )

  missing_data <- stack_validation_data
  missing_data$cases[1] <- NA_real_
  expect_error(
    gg_stack(missing_data, year, cases, fill = cancer),
    "finite, non-missing"
  )
})

test_that("gg_stack uses coordinated typography and backgrounds", {
  p <- gg_stack(
    stack_validation_data,
    year,
    cases,
    fill = cancer,
    add_label = TRUE,
    title = "癌种构成",
    subtitle = "2020—2021 年",
    font_family = "serif",
    base_size = 14,
    bg_color = "transparent"
  )

  expect_identical(p$theme$text$family, "serif")
  expect_equal(p$theme$text$size, 14)
  expect_equal(p$theme$plot.title$size, 2)
  expect_equal(p$theme$plot.subtitle$size, 1.15)
  expect_equal(as.numeric(p$theme$plot.margin), c(14, 18, 12, 18))
  expect_identical(p$theme$plot.background$fill, "transparent")
  expect_identical(p$theme$panel.background$fill, "transparent")
  expect_identical(p$layers[[2]]$aes_params$family, "serif")
})

test_that("gg_stack validates totals for proportional stacks", {
  zero_data <- stack_validation_data
  zero_data$cases[zero_data$year == 2020] <- 0

  expect_error(
    gg_stack(zero_data, year, cases, fill = cancer),
    "positive total"
  )
  expect_s3_class(
    gg_stack(zero_data, year, cases, fill = cancer, position = "stack"),
    "ggplot"
  )
})

test_that("gg_heatmap validates variables and supports Chinese facets", {
  p <- gg_heatmap(
    heatmap_validation_data,
    indicator,
    registry,
    value,
    facet = sex,
    label_format = "percent"
  )

  expect_s3_class(p, "ggplot")
  expect_error(
    gg_heatmap(heatmap_validation_data, missing, registry, value),
    "missing"
  )
  expect_error(
    gg_heatmap(
      transform(heatmap_validation_data, value = as.character(value)),
      indicator,
      registry,
      value
    ),
    "numeric column"
  )
})

test_that("gg_heatmap rejects incomplete values and invalid styles", {
  missing_data <- heatmap_validation_data
  missing_data$value[1] <- NA_real_

  expect_error(
    gg_heatmap(missing_data, indicator, registry, value),
    "finite, non-missing"
  )
  expect_error(
    gg_heatmap(
      heatmap_validation_data,
      indicator,
      registry,
      value,
      label_scale = Inf
    ),
    "label_scale"
  )
  expect_error(
    gg_heatmap(
      heatmap_validation_data,
      indicator,
      registry,
      value,
      label_size = 0
    ),
    "label_size"
  )
})

test_that("gg_heatmap uses publication typography and complete backgrounds", {
  p <- gg_heatmap(
    heatmap_validation_data,
    indicator,
    registry,
    value,
    facet = sex,
    font_family = "serif",
    base_size = 11,
    bg_color = "transparent"
  )

  expect_identical(p$theme$text$family, "serif")
  expect_equal(p$theme$text$size, 11)
  expect_identical(p$theme$plot.background$fill, "transparent")
  expect_identical(p$theme$panel.background$fill, "transparent")
  expect_identical(p$theme$strip.background$fill, "transparent")
  expect_identical(p$theme$strip.placement, "outside")
  expect_identical(p$layers[[2]]$aes_params$family, "serif")

  expect_error(
    gg_heatmap(
      heatmap_validation_data,
      indicator,
      registry,
      value,
      bg_color = "not-a-color"
    ),
    "valid R color"
  )
})
