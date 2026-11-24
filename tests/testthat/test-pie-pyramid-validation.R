pie_validation_data <- data.frame(
  cancer = c("肺癌", "胃癌", "肝癌"),
  value = c(40, 30, 20)
)

pyramid_validation_data <- data.frame(
  agegrp = rep(c("0-4岁", "5-9岁", "10-14岁"), 2),
  sex = rep(c("男", "女"), each = 3),
  cases = c(10, 20, 30, 12, 18, 25)
)

test_that("gg_pie uses shared validation without changing output", {
  p <- gg_pie(
    pie_validation_data,
    value,
    label = cancer,
    label_position = "inside",
    label_value = TRUE
  )

  expect_s3_class(p, "ggplot")
  expect_error(gg_pie(1, value), "data frame")
  expect_error(
    gg_pie(pie_validation_data, value, legend = NA),
    "legend"
  )
  expect_error(
    gg_pie(pie_validation_data, value, label_value_digits = -1),
    "label_value_digits"
  )
})

test_that("gg_pyramid supports Chinese groups and validates core inputs", {
  p <- gg_pyramid(
    pyramid_validation_data,
    cases,
    agegrp,
    sex,
    legend_labels = c("男性", "女性")
  )

  expect_s3_class(p, "patchwork")
  expect_error(gg_pyramid(1, cases, agegrp, sex), "data frame")
  expect_error(
    gg_pyramid(pyramid_validation_data, missing, agegrp, sex),
    "missing"
  )
  expect_error(
    gg_pyramid(
      transform(pyramid_validation_data, cases = as.character(cases)),
      cases,
      agegrp,
      sex
    ),
    "numeric column"
  )
})

test_that("gg_pyramid validates values and display parameters", {
  missing_data <- pyramid_validation_data
  missing_data$cases[1] <- NA_real_
  negative_data <- pyramid_validation_data
  negative_data$cases[1] <- -1

  expect_error(
    gg_pyramid(missing_data, cases, agegrp, sex),
    "finite, non-missing"
  )
  expect_error(
    gg_pyramid(negative_data, cases, agegrp, sex),
    "non-negative"
  )
  expect_error(
    gg_pyramid(pyramid_validation_data, cases, agegrp, sex, sort = "invalid"),
    "sort"
  )
  expect_error(
    gg_pyramid(
      pyramid_validation_data,
      cases,
      agegrp,
      sex,
      legend_labels = "全部"
    ),
    "legend_labels"
  )
  expect_error(
    gg_pyramid(pyramid_validation_data, cases, agegrp, sex, gap = NA_real_),
    "gap"
  )
})

test_that("pie and pyramid share publication typography and backgrounds", {
  p_pie <- gg_pie(
    pie_validation_data,
    value,
    label = cancer,
    label_position = "inside",
    title = "癌种构成",
    font_family = "serif",
    base_size = 11,
    bg_color = "transparent"
  )
  p_pyramid <- gg_pyramid(
    pyramid_validation_data,
    cases,
    agegrp,
    sex,
    legend_labels = c("男性", "女性"),
    title = "年龄构成",
    font_family = "serif",
    base_size = 11,
    bg_color = "transparent"
  )

  expect_identical(p_pie$theme$text$family, "serif")
  expect_equal(p_pie$theme$text$size, 11)
  expect_identical(p_pie$theme$plot.background$fill, "transparent")
  expect_identical(p_pie$theme$panel.background$fill, "transparent")
  expect_identical(p_pie$layers[[2]]$aes_params$family, "serif")
  expect_identical(p_pie$labels$title, "癌种构成")
  expect_s3_class(p_pie$theme$axis.text.x, "element_blank")
  expect_equal(p_pie$theme$plot.title$hjust, 0)

  expect_identical(
    p_pyramid$patches$annotation$theme$text$family,
    "serif"
  )
  expect_equal(p_pyramid$patches$annotation$theme$text$size, 11)
  expect_identical(
    p_pyramid$patches$annotation$theme$plot.background$fill,
    "transparent"
  )
  expect_identical(
    p_pyramid$patches$plots[[1]]$theme$panel.background$fill,
    "transparent"
  )
  left_pyramid_panel <- p_pyramid$patches$plots[[1]]$patches$plots[[1]]
  right_pyramid_panel <- p_pyramid$patches$plots[[1]]
  expect_s3_class(
    left_pyramid_panel$theme$axis.text.y.left,
    "element_blank"
  )
  expect_s3_class(
    right_pyramid_panel$theme$axis.text.y.left,
    "element_text"
  )
  expect_equal(right_pyramid_panel$theme$axis.text.y.left$hjust, 0.5)
  expect_equal(right_pyramid_panel$theme$axis.text.y.left$angle, 0)
  expect_equal(
    as.numeric(right_pyramid_panel$theme$axis.text.y.left$margin),
    c(0, 12, 0, 0)
  )
  expect_identical(left_pyramid_panel$theme$plot.title.position, "panel")
  expect_identical(right_pyramid_panel$theme$plot.title.position, "panel")
  expect_equal(left_pyramid_panel$theme$plot.title$hjust, 1)
  expect_equal(right_pyramid_panel$theme$plot.title$hjust, 0)
  expect_no_warning(
    ggplot2::ggplot_build(p_pyramid$patches$plots[[1]])
  )

  expect_error(
    gg_pie(pie_validation_data, value, bg_color = "not-a-color"),
    "valid R color"
  )
  expect_error(
    gg_pyramid(
      pyramid_validation_data,
      cases,
      agegrp,
      sex,
      bg_color = 1
    ),
    "single character"
  )
})

test_that("gg_pyramid displays each facet label at the upper left", {
  facet_data <- dplyr::bind_rows(
    transform(pyramid_validation_data, area = "城区"),
    transform(pyramid_validation_data, area = "农村")
  )
  p <- gg_pyramid(
    facet_data,
    cases,
    agegrp,
    sex,
    facet = area,
    ncol = 2
  )

  collect_tags <- function(plot) {
    tag <- plot$labels$tag
    nested <- plot$patches$plots
    c(
      if (is.null(tag)) character() else tag,
      unlist(lapply(nested, collect_tags), use.names = FALSE)
    )
  }

  expect_setequal(collect_tags(p), c("城区", "农村"))
})
