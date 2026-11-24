plot_data <- data.frame(
  year = rep(2020:2022, 2),
  sex = rep(c("男", "女"), each = 3),
  cancer = rep(c("肺癌", "乳腺癌", "胃癌"), 2),
  incidence = c(45.2, 38.1, 22.5, 52.3, 41.4, 26.8),
  mortality = c(35.1, 8.2, 18.5, 42.4, 9.1, 20.2)
)

test_that("gg_barchart returns a ggplot object", {
  p <- gg_barchart(plot_data, cancer, incidence)
  expect_s3_class(p, "ggplot")
})

test_that("gg_barchart supports unified labels and grouped palette", {
  p <- gg_barchart(
    plot_data,
    cancer,
    incidence,
    group = sex,
    palette = "sex",
    add_label = TRUE,
    label_digits = 1,
    title = "发病率",
    x_lab = NULL,
    y_lab = "1/10万",
    legend_title = "性别"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "发病率")
  expect_equal(p$labels$y, "1/10万")
  expect_equal(p$labels$fill, "性别")
})

test_that("gg_barchart supports count mode, highlight, and facets", {
  p_count <- gg_barchart(plot_data, cancer)
  expect_s3_class(p_count, "ggplot")

  p_highlight <- gg_barchart(plot_data, cancer, incidence, highlight = "肺癌")
  expect_s3_class(p_highlight, "ggplot")

  p_facet <- gg_barchart(plot_data, cancer, incidence, facet = sex, top_n = 2)
  expect_s3_class(p_facet, "ggplot")
  expect_identical(p_facet$facet$params$strip.position, "top")
  expect_identical(p_facet$theme$strip.placement, "outside")

  layout_data <- data.frame(
    facet = rep(c("A", "B"), each = 2),
    category = rep(c("One", "Two"), 2),
    value = 1:4
  )
  p_layout <- gg_barchart(layout_data, category, value, facet = facet)
  grob <- ggplot2::ggplotGrob(p_layout)
  first_strip_row <- min(grob$layout$t[grepl("^strip-t", grob$layout$name)])
  first_top_axis_row <- min(grob$layout$t[grepl("^axis-t", grob$layout$name)])
  expect_lt(first_strip_row, first_top_axis_row)
})

test_that("gg_linechart returns a ggplot object", {
  p <- gg_linechart(plot_data, year, incidence, group = sex)
  expect_s3_class(p, "ggplot")
})

test_that("gg_linechart reports missing variables clearly", {
  expect_error(
    gg_linechart(plot_data, year, death, group = sex),
    "变量 death 不存在于 `data` 中"
  )

  expect_error(
    gg_linechart(plot_data, year, c(incidence, death)),
    "变量 death 不存在于 `data` 中"
  )
})

test_that("gg_linechart supports unified labels and palette", {
  p <- gg_linechart(
    plot_data,
    year,
    incidence,
    group = sex,
    palette = "sex",
    add_label = TRUE,
    label_digits = 1,
    title = "年度趋势",
    x_lab = "年份",
    y_lab = "发病率（1/10万）",
    legend_title = "性别"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "年度趋势")
  expect_equal(p$labels$x, "年份")
  expect_equal(p$labels$y, "发病率（1/10万）")
  expect_equal(p$labels$colour, "性别")
})

test_that("gg_linechart uses coordinated typography and spacing", {
  p <- gg_linechart(
    plot_data,
    year,
    incidence,
    group = sex,
    title = "年度趋势",
    x_lab = "年份"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(as.numeric(p$theme$plot.margin), c(14, 18, 12, 18))
  expect_equal(as.numeric(p$theme$plot.title$margin), c(0, 0, 14, 0))
  expect_equal(as.numeric(p$theme$plot.subtitle$margin), c(0, 0, 14, 0))
  expect_equal(p$theme$plot.title$size, 2)
  expect_equal(p$theme$plot.subtitle$size, 1.15)
  expect_equal(as.numeric(p$theme$axis.text.x.bottom$margin), c(4, 0, 0, 0))
  expect_equal(as.numeric(p$theme$axis.text.y.left$margin), c(0, 4, 0, 0))
  expect_equal(as.numeric(p$theme$axis.title.x.bottom$margin), c(7, 0, 0, 0))
  expect_equal(as.numeric(p$theme$axis.title.y.left$margin), c(0, 7, 0, 0))
  expect_identical(p$theme$plot.title.position, "plot")
  expect_identical(p$theme$text$family, "sans")
  expect_equal(p$theme$text$size, 12)

  p_subtitle <- gg_linechart(
    plot_data,
    year,
    incidence,
    title = "年度趋势",
    subtitle = "2020—2022 年",
    x_lab = "年份"
  )

  expect_equal(as.numeric(p_subtitle$theme$plot.title$margin), c(0, 0, 4, 0))
})

test_that("line and bar charts apply custom fonts to plot and labels", {
  p_line <- gg_linechart(
    plot_data,
    year,
    incidence,
    add_label = TRUE,
    font_family = "serif",
    base_size = 11
  )
  p_bar <- gg_barchart(
    plot_data,
    cancer,
    incidence,
    add_label = TRUE,
    font_family = "serif",
    base_size = 11
  )

  expect_identical(p_line$theme$text$family, "serif")
  expect_equal(p_line$theme$text$size, 11)
  expect_identical(p_line$layers[[3]]$aes_params$family, "serif")
  expect_identical(p_bar$theme$text$family, "serif")
  expect_equal(p_bar$theme$text$size, 11)
  expect_identical(p_bar$layers[[2]]$aes_params$family, "serif")
  expect_equal(as.numeric(p_bar$theme$plot.margin), c(14, 18, 12, 18))

  expect_error(
    gg_linechart(plot_data, year, incidence, base_size = 0),
    "greater than 0"
  )
  expect_error(
    gg_barchart(plot_data, cancer, incidence, font_family = c("sans", "serif")),
    "single character"
  )
})

test_that("line and bar charts support complete figure backgrounds", {
  p_line <- gg_linechart(
    plot_data,
    year,
    incidence,
    bg_color = "#F4F1EA"
  )
  p_bar <- gg_barchart(
    plot_data,
    cancer,
    incidence,
    facet = sex,
    bg_color = "transparent"
  )

  expect_identical(p_line$theme$plot.background$fill, "#F4F1EA")
  expect_identical(p_line$theme$panel.background$fill, "#F4F1EA")
  expect_identical(p_bar$theme$plot.background$fill, "transparent")
  expect_identical(p_bar$theme$panel.background$fill, "transparent")
  expect_identical(p_bar$theme$strip.background$fill, "transparent")

  expect_snapshot(
    error = TRUE,
    gg_linechart(plot_data, year, incidence, bg_color = 1)
  )
  expect_snapshot(
    error = TRUE,
    gg_barchart(plot_data, cancer, incidence, bg_color = "not-a-color")
  )
})

test_that("gg_linechart supports wide data and facet legends", {
  p_wide <- gg_linechart(plot_data, year, c(incidence, mortality))
  expect_s3_class(p_wide, "ggplot")

  p_breaks <- gg_linechart(
    plot_data,
    year,
    incidence,
    x_breaks = 2020:2022,
    x_labels = paste0("Y", 2020:2022)
  )
  expect_s3_class(p_breaks, "ggplot")

  p_facet_legend <- gg_linechart(
    plot_data,
    year,
    incidence,
    group = sex,
    facet = cancer,
    facet_legend = TRUE,
    legend_position = "topright"
  )
  expect_s3_class(p_facet_legend, "ggplot")
})

test_that("gg_dumbbell returns a ggplot object", {
  data <- data.frame(
    cancer = rep(c("肺癌", "胃癌", "肝癌"), each = 2),
    sex = rep(c("男", "女"), 3),
    rate = c(55, 38, 28, 18, 31, 12)
  )

  p <- gg_dumbbell(data, cancer, rate, sex)
  expect_s3_class(p, "ggplot")
})

test_that("gg_dumbbell validates groups and supports optional diff labels", {
  data <- data.frame(
    cancer = rep(c("肺癌", "胃癌", "肝癌"), each = 2),
    sex = rep(c("男", "女"), 3),
    rate = c(55, 38, 28, 18, 31, 12)
  )

  p <- gg_dumbbell(data, cancer, rate, sex, diff_label = FALSE, palette = "sex")
  expect_s3_class(p, "ggplot")

  p_label_digits <- gg_dumbbell(data, cancer, rate, sex, label_digits = 0)
  expect_s3_class(p_label_digits, "ggplot")

  p_ordered <- gg_dumbbell(
    data,
    cancer,
    rate,
    sex,
    group_order = c("男", "女"),
    diff_direction = "last_minus_first"
  )
  expect_s3_class(p_ordered, "ggplot")

  bad_data <- rbind(data, data.frame(cancer = "肺癌", sex = "合计", rate = 45))
  expect_error(
    gg_dumbbell(bad_data, cancer, rate, sex),
    "exactly two unique values"
  )
})

test_that("gg_dumbbell sort controls the displayed category order", {
  data <- data.frame(
    cancer = rep(c("A", "B", "C"), each = 2),
    sex = rep(c("first", "last"), 3),
    rate = c(10, 2, 20, 8, 5, 4)
  )

  p_desc <- gg_dumbbell(
    data,
    cancer,
    rate,
    sex,
    sort = "desc",
    sort_by = "last"
  )
  p_asc <- gg_dumbbell(
    data,
    cancer,
    rate,
    sex,
    sort = "asc",
    sort_by = "last"
  )
  p_vertical <- gg_dumbbell(
    data,
    cancer,
    rate,
    sex,
    sort = "desc",
    sort_by = "last",
    horizontal = FALSE
  )

  get_axis_limits <- function(plot, axis) {
    ggplot2::ggplot_build(plot)$layout$panel_params[[1]][[axis]]$get_limits()
  }

  expect_equal(get_axis_limits(p_desc, "y"), c("A", "C", "B"))
  expect_equal(get_axis_limits(p_asc, "y"), c("B", "C", "A"))
  expect_equal(get_axis_limits(p_vertical, "x"), c("B", "C", "A"))
})

test_that("gg_dumbbell sorts diff by absolute difference", {
  data <- data.frame(
    cancer = rep(c("A", "B", "C", "D"), each = 2),
    sex = rep(c("first", "last"), 4),
    rate = c(10, 2, 1, 10, 20, 17, 5, 12)
  )

  get_axis_limits <- function(plot) {
    ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$y$get_limits()
  }

  p_desc <- gg_dumbbell(
    data,
    cancer,
    rate,
    sex,
    sort = "desc",
    sort_by = "diff"
  )
  p_asc <- gg_dumbbell(
    data,
    cancer,
    rate,
    sex,
    sort = "asc",
    sort_by = "diff"
  )
  p_top <- gg_dumbbell(
    data,
    cancer,
    rate,
    sex,
    sort = "desc",
    sort_by = "diff",
    top_n = 2
  )

  expect_equal(get_axis_limits(p_desc), c("C", "D", "A", "B"))
  expect_equal(get_axis_limits(p_asc), c("B", "A", "D", "C"))
  expect_equal(get_axis_limits(p_top), c("A", "B"))
})

test_that("gg_dual_barchart and legacy alias return patchwork objects", {
  data <- data.frame(
    cancer = rep(c("肺癌", "胃癌", "肝癌"), 2),
    indicator = rep(c("Incidence", "Mortality"), each = 3),
    rate = c(55, 28, 31, 42, 20, 18)
  )

  expect_s3_class(gg_dual_barchart(data, cancer, rate, indicator), "patchwork")
  expect_s3_class(gg_dul_barchart(data, cancer, rate, indicator), "patchwork")

  grouped_data <- data.frame(
    cancer = rep(c("肺癌", "胃癌", "肝癌"), 4),
    indicator = rep(c("Incidence", "Mortality"), each = 6),
    sex = rep(c("男", "女"), each = 3, times = 2),
    rate = c(55, 28, 31, 38, 18, 12, 42, 20, 18, 20, 10, 8)
  )
  p_grouped <- gg_dual_barchart(
    grouped_data,
    cancer,
    rate,
    indicator,
    fill = sex
  )
  expect_s3_class(p_grouped, "patchwork")
  expect_identical(p_grouped$patches$layout$guides, "collect")
  expect_identical(p_grouped$theme$legend.position, "top")
})

test_that("gg_pie returns a ggplot object", {
  p <- gg_pie(plot_data[1:3, ], incidence, label = cancer)
  expect_s3_class(p, "ggplot")

  p_donut <- gg_pie(
    plot_data[1:3, ],
    incidence,
    label = cancer,
    inner_radius = 0.9,
    label_position = "inside",
    label_radius = 1.25
  )
  expect_s3_class(p_donut, "ggplot")

  p_highlight <- gg_pie(
    plot_data[1:3, ],
    incidence,
    label = cancer,
    label_position = "inside",
    highlight = "肺癌"
  )
  expect_s3_class(p_highlight, "ggplot")

  p_outside <- gg_pie(
    plot_data[1:3, ],
    incidence,
    label = cancer,
    label_position = "outside"
  )
  expect_s3_class(p_outside, "ggplot")

  expect_error(
    gg_pie(plot_data[1:3, ], incidence, label = cancer, inner_radius = -0.1),
    "inner_radius"
  )
  expect_error(
    gg_pie(plot_data[1:3, ], incidence, label = cancer, inner_radius = 1.5),
    "inner_radius"
  )
})

test_that("gg_pie supports facets with panel-wise slice totals", {
  p <- gg_pie(plot_data, incidence, label = cancer, facet = sex)
  expect_s3_class(p, "ggplot")

  slice_data <- ggplot2::ggplot_build(p)$data[[1]]
  panel_totals <- tapply(slice_data$ymax, slice_data$PANEL, max)

  expect_equal(as.numeric(panel_totals), rep(1, length(panel_totals)))
})

test_that("gg_pie makes faceted inside-label pies compact", {
  p <- gg_pie(
    plot_data,
    incidence,
    label = cancer,
    facet = sex,
    label_position = "inside"
  )

  expect_equal(as.numeric(p$theme$panel.spacing), 2)
  expect_equal(as.numeric(p$theme$plot.margin), c(4, 4, 4, 4))
  expect_equal(p$scales$get_scales("x")$limits, c(0, 1.5))
})

test_that("gg_pie normalizes raw values independently within facets", {
  pie_data <- data.frame(
    sex = rep(c("男", "女"), each = 3),
    cancer = rep(c("A", "B", "C"), 2),
    value = c(100, 50, 50, 10, 5, 5)
  )

  p <- gg_pie(
    pie_data,
    value,
    label = cancer,
    facet = sex,
    label_position = "inside",
    label_value = TRUE,
    label_value_digits = 0
  )

  expect_s3_class(p, "ggplot")

  built <- ggplot2::ggplot_build(p)
  slice_data <- built$data[[1]]
  panel_totals <- tapply(slice_data$ymax, slice_data$PANEL, max)

  expect_equal(as.numeric(panel_totals), rep(1, length(panel_totals)))

  plot_data_built <- built$plot$data
  percent_by_sex <- split(
    plot_data_built$.ggcanreg_pie_label_percent,
    plot_data_built$sex
  )

  expect_equal(unname(percent_by_sex[["男"]]), c(50, 25, 25))
  expect_equal(unname(percent_by_sex[["女"]]), c(50, 25, 25))
})

test_that("gg_pie applies top_n within each facet", {
  pie_data <- data.frame(
    sex = rep(c("男", "女"), each = 4),
    cancer = rep(c("A", "B", "C", "D"), 2),
    value = c(100, 90, 1, 1, 1, 1, 100, 90)
  )

  p <- gg_pie(pie_data, value, label = cancer, facet = sex, top_n = 2)
  expect_s3_class(p, "ggplot")

  plot_data_top <- ggplot2::ggplot_build(p)$plot$data
  label_by_sex <- split(as.character(plot_data_top$cancer), plot_data_top$sex)
  value_by_sex <- split(plot_data_top$value, plot_data_top$sex)

  expect_equal(label_by_sex[["男"]], c("A", "B", "其他"))
  expect_equal(label_by_sex[["女"]], c("C", "D", "其他"))
  expect_equal(value_by_sex[["男"]], c(100, 90, 2))
  expect_equal(value_by_sex[["女"]], c(100, 90, 2))
})

test_that("gg_pie uses discrete default palette", {
  pie_data <- data.frame(
    cancer = c("A", "B", "C"),
    value = c(2, 5, 1)
  )

  p <- gg_pie(pie_data, value, label = cancer)
  fill_values <- ggplot2::ggplot_build(p)$data[[1]]$fill

  expect_equal(fill_values, unname(canreg_palette("default", n = 3)))
})

test_that("gg_pie supports ggcanreg and legacy colorspace palettes", {
  sex_data <- data.frame(
    sex = c("男", "女"),
    value = c(60, 40)
  )

  p_sex <- gg_pie(sex_data, value, label = sex, palette = "sex")
  expect_s3_class(p_sex, "ggplot")
  expect_equal(
    ggplot2::ggplot_build(p_sex)$data[[1]]$fill,
    unname(canreg_palette("sex", n = 2))
  )

  p_blues <- gg_pie(sex_data, value, label = sex, palette = "Blues")
  expect_s3_class(p_blues, "ggplot")
})

test_that("gg_pie inner_radius keeps zero as polar center", {
  p <- gg_pie(
    plot_data[1:3, ],
    incidence,
    label = cancer,
    inner_radius = 0.9
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$scales$get_scales("x")$limits[1], 0)
})

test_that("gg_pie supports center label", {
  p <- gg_pie(
    plot_data[1:3, ],
    incidence,
    label = cancer,
    inner_radius = 0.9,
    center_label = "合计",
    center_label_size = 5,
    center_label_color = "grey20"
  )

  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomText"),
    logical(1)
  )))
})

test_that("gg_pie uses facet values as default center labels", {
  p <- gg_pie(plot_data, incidence, label = cancer, facet = sex)
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$theme$strip.text, "element_blank")
  expect_s3_class(p$theme$strip.background, "element_blank")

  text_layers <- ggplot2::ggplot_build(p)$data[vapply(
    ggplot2::ggplot_build(p)$data,
    function(layer_data) "label" %in% names(layer_data),
    logical(1)
  )]
  center_text <- text_layers[[1]]

  expect_equal(sort(center_text$label), sort(unique(plot_data$sex)))
})

test_that("gg_pie supports ncol for faceted pies", {
  p <- gg_pie(plot_data, incidence, label = cancer, facet = sex, ncol = 1)
  expect_s3_class(p, "ggplot")

  layout_data <- ggplot2::ggplot_build(p)$layout$layout
  expect_equal(max(layout_data$COL), 1)

  expect_error(
    gg_pie(plot_data, incidence, label = cancer, facet = sex, ncol = 0),
    "ncol"
  )
  expect_error(
    gg_pie(plot_data, incidence, label = cancer, facet = sex, ncol = 1.5),
    "ncol"
  )
})

test_that("gg_pie supports vector center labels with facets", {
  p <- gg_pie(
    plot_data,
    incidence,
    label = cancer,
    facet = sex,
    center_label = c("男性", "女性")
  )
  expect_s3_class(p, "ggplot")

  text_layers <- ggplot2::ggplot_build(p)$data[vapply(
    ggplot2::ggplot_build(p)$data,
    function(layer_data) "label" %in% names(layer_data),
    logical(1)
  )]
  center_text <- text_layers[[1]]

  expect_equal(center_text$label, c("男性", "女性"))

  p_same <- gg_pie(
    plot_data,
    incidence,
    label = cancer,
    facet = sex,
    center_label = "合计"
  )
  same_text_layers <- ggplot2::ggplot_build(p_same)$data[vapply(
    ggplot2::ggplot_build(p_same)$data,
    function(layer_data) "label" %in% names(layer_data),
    logical(1)
  )]

  expect_equal(same_text_layers[[1]]$label, c("合计", "合计"))

  expect_error(
    gg_pie(
      plot_data,
      incidence,
      label = cancer,
      facet = sex,
      center_label = c("A", "B", "C")
    ),
    "center_label"
  )
  expect_error(
    gg_pie(
      plot_data[1:3, ],
      incidence,
      label = cancer,
      center_label = c("A", "B")
    ),
    "center_label"
  )
})

test_that("gg_pie can append values to labels", {
  pie_data <- data.frame(
    cancer = c("A", "B", "C", "D"),
    value = c(23.44, 20, 10, 5)
  )

  p <- gg_pie(
    pie_data,
    value,
    label = cancer,
    label_position = "inside",
    label_value = TRUE,
    label_value_digits = 1
  )
  text_data <- ggplot2::ggplot_build(p)$data[[2]]

  expect_true(any(grepl("\\n", text_data$label)))
  expect_true("A\n40.1%" %in% text_data$label)

  p_both <- gg_pie(
    pie_data,
    value,
    label = cancer,
    label_position = "inside",
    label_value = TRUE,
    label_value_type = c("value", "percent"),
    label_value_digits = 1
  )
  both_text_data <- ggplot2::ggplot_build(p_both)$data[[2]]

  expect_true("A\n23.44, 40.1%" %in% both_text_data$label)

  p_both_reversed <- gg_pie(
    pie_data,
    value,
    label = cancer,
    label_position = "inside",
    label_value = TRUE,
    label_value_type = c("percent", "value"),
    label_value_digits = 1
  )
  both_reversed_text_data <- ggplot2::ggplot_build(p_both_reversed)$data[[2]]

  expect_true("A\n40.1%, 23.44" %in% both_reversed_text_data$label)

  p_value <- gg_pie(
    pie_data,
    value,
    label = cancer,
    label_position = "inside",
    label_value = TRUE,
    label_value_type = "value",
    label_value_digits = 1,
    label_value_suffix = " cases"
  )
  value_text_data <- ggplot2::ggplot_build(p_value)$data[[2]]

  expect_true("A\n23.44 cases" %in% value_text_data$label)

  p_top <- gg_pie(
    pie_data,
    value,
    label = cancer,
    label_position = "inside",
    top_n = 2,
    label_value = TRUE,
    label_value_type = c("value", "percent"),
    label_value_digits = 0
  )
  top_text_data <- ggplot2::ggplot_build(p_top)$data[[2]]

  expect_true(any(grepl("15, 26%", top_text_data$label, fixed = TRUE)))
})

test_that("gg_pie supports slice sorting", {
  pie_data <- data.frame(
    cancer = c("A", "B", "C"),
    value = c(2, 5, 1)
  )

  p_none <- gg_pie(pie_data, value, label = cancer, sort = "none")
  p_desc <- gg_pie(pie_data, value, label = cancer, sort = "desc")
  p_asc <- gg_pie(pie_data, value, label = cancer, sort = "asc")

  get_slice_values <- function(p) {
    layer_data <- ggplot2::ggplot_build(p)$data[[1]]
    layer_data$ymax - layer_data$ymin
  }

  expect_equal(get_slice_values(p_none), c(2, 5, 1))
  expect_equal(get_slice_values(p_desc), c(5, 2, 1))
  expect_equal(get_slice_values(p_asc), c(1, 2, 5))
})

test_that("gg_pie can place other groups at the end", {
  pie_data <- data.frame(
    cancer = c("A", "其他", "B", "C"),
    value = c(2, 1, 5, 3)
  )

  get_slice_values <- function(p) {
    layer_data <- ggplot2::ggplot_build(p)$data[[1]]
    layer_data$ymax - layer_data$ymin
  }

  p_none <- gg_pie(
    pie_data,
    value,
    label = cancer,
    sort = "none",
    other_group = "其他"
  )
  expect_equal(get_slice_values(p_none), c(2, 5, 3, 1))

  p_desc <- gg_pie(
    pie_data,
    value,
    label = cancer,
    sort = "desc",
    other_group = "其他"
  )
  expect_equal(get_slice_values(p_desc), c(5, 3, 2, 1))

  p_asc <- gg_pie(
    pie_data,
    value,
    label = cancer,
    sort = "asc",
    other_group = "其他"
  )
  expect_equal(get_slice_values(p_asc), c(1, 2, 3, 5))

  expect_error(
    gg_pie(pie_data, value, other_group = "其他"),
    "other_group"
  )
})

test_that("gg_pie can merge slices after top_n", {
  pie_data <- data.frame(
    cancer = c("A", "B", "C", "D", "E"),
    value = c(2, 5, 1, 4, 3)
  )

  get_slice_values <- function(p) {
    layer_data <- ggplot2::ggplot_build(p)$data[[1]]
    layer_data$ymax - layer_data$ymin
  }

  p_none <- gg_pie(
    pie_data,
    value,
    label = cancer,
    top_n = 3,
    sort = "none"
  )
  expect_equal(get_slice_values(p_none), c(5, 4, 3, 3))

  p_asc <- gg_pie(
    pie_data,
    value,
    label = cancer,
    top_n = 3,
    sort = "asc"
  )
  expect_equal(get_slice_values(p_asc), c(3, 3, 4, 5))

  other_data <- data.frame(
    cancer = c("A", "其他", "B", "C", "D"),
    value = c(2, 10, 5, 1, 4)
  )
  p_with_other <- gg_pie(
    other_data,
    value,
    label = cancer,
    top_n = 2,
    other_group = "其他",
    sort = "desc"
  )
  expect_equal(get_slice_values(p_with_other), c(5, 4, 13))

  expect_error(
    gg_pie(pie_data, value, top_n = 3),
    "top_n"
  )
  expect_error(
    gg_pie(pie_data, value, label = cancer, top_n = 0),
    "top_n"
  )
})

test_that("gg_pie keeps label colors stable after sorting", {
  pie_data <- data.frame(
    cancer = c("A", "B", "C"),
    value = c(2, 5, 1)
  )

  get_color_by_value <- function(p) {
    layer_data <- ggplot2::ggplot_build(p)$data[[1]]
    values <- layer_data$ymax - layer_data$ymin
    stats::setNames(layer_data$fill, values)
  }

  colors_desc <- get_color_by_value(
    gg_pie(pie_data, value, label = cancer, sort = "desc")
  )
  colors_asc <- get_color_by_value(
    gg_pie(pie_data, value, label = cancer, sort = "asc")
  )

  expect_equal(colors_desc["2"], colors_asc["2"])
  expect_equal(colors_desc["5"], colors_asc["5"])
  expect_equal(colors_desc["1"], colors_asc["1"])
})

test_that("gg_pie supports outside label line settings", {
  pie_data <- data.frame(
    cancer = c("A", "B", "C", "D"),
    value = c(80, 8, 7, 5)
  )

  p <- gg_pie(
    pie_data,
    value,
    label = cancer,
    label_position = "outside",
    outside_line_length = 0.4,
    outside_label_gap = 0.12,
    outside_line_width = 0.6,
    outside_line_type = "dashed"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$layers[[2]]$aes_params$linewidth, 0.6)
  expect_equal(p$layers[[2]]$aes_params$linetype, "dashed")
  expect_false(is.null(p$layers[[3]]$mapping$hjust))
})

test_that("gg_pie can repel outside labels", {
  pie_data <- data.frame(
    cancer = c("A", "B", "C", "D"),
    value = c(90, 4, 3, 3)
  )

  p_repel <- gg_pie(
    pie_data,
    value,
    label = cancer,
    label_position = "outside",
    outside_label_repel = TRUE,
    outside_label_min_gap = 0.1
  )

  built <- ggplot2::ggplot_build(p_repel)$data
  segment_data <- built[[2]]

  expect_true(any(segment_data$y != segment_data$yend))
})

test_that("gg_comparison returns a ggplot object", {
  p <- gg_comparison(plot_data, incidence, cancer, highlight = "肺癌")
  expect_s3_class(p, "ggplot")
})

test_that("gg_big_value returns a ggplot object", {
  p <- gg_big_value(45.2, "发病率", value_suffix = "/10万")
  expect_s3_class(p, "ggplot")
})

test_that("gg_heatmap returns a ggplot object with unified labels", {
  data <- data.frame(
    registry = rep(c("A", "B"), each = 2),
    indicator = rep(c("MV", "DCO"), 2),
    value = c(75.2, 2.1, 68.4, 3.5)
  )

  p <- gg_heatmap(
    data,
    indicator,
    registry,
    value,
    title = "质量指标",
    legend_title = "%"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "质量指标")
  expect_equal(p$labels$fill, "%")

  p_percent <- gg_heatmap(
    data,
    indicator,
    registry,
    value,
    label_format = "percent",
    label_scale = 1
  )
  expect_s3_class(p_percent, "ggplot")
})

test_that("gg_big_value supports value_fun", {
  p <- gg_big_value(45.2, "发病率", value_fun = function(x) format_rate(x, 1))
  expect_s3_class(p, "ggplot")

  expect_error(
    gg_big_value(45.2, "发病率", value_fun = "not-a-function"),
    "value_fun"
  )
})

test_that("gg_pyramid returns a patchwork object and validates groups", {
  data <- data.frame(
    agegrp = rep(c("0-4", "5-9", "10-14"), 2),
    sex = rep(c("男", "女"), each = 3),
    cases = c(10, 20, 30, 12, 18, 25)
  )

  p <- gg_pyramid(data, cases, agegrp, sex, palette = "sex")
  expect_s3_class(p, "patchwork")

  bad_data <- rbind(data, data.frame(agegrp = "0-4", sex = "合计", cases = 22))
  expect_error(
    gg_pyramid(bad_data, cases, agegrp, sex),
    "exactly two unique values"
  )
})

test_that("gg_stack returns a ggplot object", {
  stack_data <- data.frame(
    year = rep(2020:2022, each = 3),
    cancer = rep(c("Lung", "Stomach", "Liver"), 3),
    cases = c(45, 28, 31, 42, 25, 28, 40, 22, 30)
  )

  p <- gg_stack(stack_data, year, cases, fill = cancer)
  expect_s3_class(p, "ggplot")

  # Stacked area chart
  p_area <- gg_stack(stack_data, year, cases, fill = cancer, type = "area")
  expect_s3_class(p_area, "ggplot")

  # Raw-value stacking
  p_raw <- gg_stack(stack_data, year, cases, fill = cancer, position = "stack")
  expect_s3_class(p_raw, "ggplot")
})

test_that("gg_stack validates arguments", {
  stack_data <- data.frame(
    year = rep(2020:2022, each = 3),
    cancer = rep(c("Lung", "Stomach", "Liver"), 3),
    cases = c(45, 28, 31, 42, 25, 28, 40, 22, 30)
  )

  expect_error(
    gg_stack(stack_data, year, cases, fill = cancer, type = "unknown"),
    "should be one of"
  )
  expect_error(
    gg_stack(stack_data, year, cases, fill = cancer, position = "unknown"),
    "should be one of"
  )
  expect_error(
    gg_stack("not_a_df", year, cases, fill = cancer),
    "must be a data frame"
  )
  expect_error(
    gg_stack(stack_data, year, cases, fill = cancer, label_digits = -1),
    "label_digits"
  )
  expect_error(
    gg_stack(stack_data, year, cases, fill = cancer, bar_width = 1.5),
    "bar_width"
  )
})

test_that("gg_stack supports labels and unified annotations", {
  stack_data <- data.frame(
    year = rep(2020:2022, each = 3),
    cancer = rep(c("Lung", "Stomach", "Liver"), 3),
    cases = c(45, 28, 31, 42, 25, 28, 40, 22, 30)
  )

  p <- gg_stack(
    stack_data,
    year,
    cases,
    fill = cancer,
    add_label = TRUE,
    label_digits = 1,
    title = "Composition",
    x_lab = "Year",
    y_lab = "%",
    legend_title = "Site",
    x_angle = 45
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Composition")
  expect_equal(p$labels$x, "Year")
  expect_equal(p$labels$y, "%")
  expect_equal(p$labels$fill, "Site")
})

test_that("gg_stack supports axis title aliases and legend position", {
  stack_data <- data.frame(
    year = rep(2020:2022, each = 3),
    cancer = rep(c("Lung", "Stomach", "Liver"), 3),
    cases = c(45, 28, 31, 42, 25, 28, 40, 22, 30)
  )

  p_alias <- gg_stack(
    stack_data,
    year,
    cases,
    fill = cancer,
    axis_title_x = "Axis Year",
    axis_title_y = "Axis Percent",
    legend_position = "bottom"
  )

  expect_s3_class(p_alias, "ggplot")
  expect_equal(p_alias$labels$x, "Axis Year")
  expect_equal(p_alias$labels$y, "Axis Percent")
  expect_equal(p_alias$theme$legend.position, "bottom")

  p_preferred <- gg_stack(
    stack_data,
    year,
    cases,
    fill = cancer,
    x_lab = "Preferred Year",
    y_lab = "Preferred Percent",
    axis_title_x = "Axis Year",
    axis_title_y = "Axis Percent"
  )

  expect_equal(p_preferred$labels$x, "Preferred Year")
  expect_equal(p_preferred$labels$y, "Preferred Percent")

  expect_error(
    gg_stack(stack_data, year, cases, fill = cancer, legend_position = NA),
    "legend_position"
  )
})

test_that("gg_stack supports horizontal and reverse options", {
  stack_data <- data.frame(
    year = rep(2020:2022, each = 3),
    cancer = rep(c("Lung", "Stomach", "Liver"), 3),
    cases = c(45, 28, 31, 42, 25, 28, 40, 22, 30)
  )

  p_vertical <- gg_stack(
    stack_data,
    year,
    cases,
    fill = cancer,
    horizontal = FALSE
  )
  expect_s3_class(p_vertical, "ggplot")

  p_reverse <- gg_stack(
    stack_data,
    year,
    cases,
    fill = cancer,
    reverse = TRUE
  )
  expect_s3_class(p_reverse, "ggplot")

  p_vertical_reverse <- gg_stack(
    stack_data,
    year,
    cases,
    fill = cancer,
    horizontal = FALSE,
    reverse = TRUE
  )
  expect_s3_class(p_vertical_reverse, "ggplot")
})

test_that("gg_stack uses discrete default palette with consistent colors", {
  stack_data <- data.frame(
    year = rep(2020:2022, each = 3),
    cancer = rep(c("Lung", "Stomach", "Liver"), 3),
    cases = c(45, 28, 31, 42, 25, 28, 40, 22, 30)
  )

  p <- gg_stack(stack_data, year, cases, fill = cancer)
  fill_values <- sort(unique(ggplot2::ggplot_build(p)$data[[1]]$fill))

  expect_equal(fill_values, sort(unname(canreg_palette("default", n = 3))))
})

test_that("gg_stack can hide legend", {
  stack_data <- data.frame(
    year = rep(2020:2022, each = 3),
    cancer = rep(c("Lung", "Stomach", "Liver"), 3),
    cases = c(45, 28, 31, 42, 25, 28, 40, 22, 30)
  )

  p_legend_hidden <- gg_stack(
    stack_data,
    year,
    cases,
    fill = cancer,
    legend = FALSE
  )
  built <- ggplot2::ggplot_build(p_legend_hidden)
  expect_s3_class(p_legend_hidden, "ggplot")
})

test_that("gg_stack supports legend style parameters", {
  stack_data <- data.frame(
    year = rep(2020:2022, each = 3),
    cancer = rep(c("Lung", "Stomach", "Liver"), 3),
    cases = c(45, 28, 31, 42, 25, 28, 40, 22, 30)
  )

  p <- gg_stack(
    stack_data,
    year,
    cases,
    fill = cancer,
    legend_title = "Site",
    legend_direction = "horizontal",
    legend_nrow = 1,
    legend_title_size = 9,
    legend_text_size = 8,
    legend_key_width = 1.2,
    legend_key_height = grid::unit(0.8, "lines")
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$fill, "Site")
  expect_equal(p$theme$legend.direction, "horizontal")
  expect_equal(p$theme$legend.title$size, 9)
  expect_equal(p$theme$legend.text$size, 8)
  expect_s3_class(p$theme$legend.key.width, "unit")
  expect_s3_class(p$theme$legend.key.height, "unit")

  expect_error(
    gg_stack(stack_data, year, cases, fill = cancer, legend_direction = "diag"),
    "should be one of"
  )
  expect_error(
    gg_stack(stack_data, year, cases, fill = cancer, legend_nrow = 0),
    "legend_nrow"
  )
  expect_error(
    gg_stack(stack_data, year, cases, fill = cancer, legend_text_size = -1),
    "legend_text_size"
  )
  expect_error(
    gg_stack(stack_data, year, cases, fill = cancer, legend_key_width = -1),
    "legend_key_width"
  )
})

test_that("legend style parameters are shared by main plots", {
  p_bar <- gg_barchart(
    plot_data,
    cancer,
    incidence,
    group = sex,
    legend_title = "Sex",
    legend_direction = "horizontal",
    legend_nrow = 1,
    legend_title_size = 9,
    legend_text_size = 8,
    legend_key_width = 1.2
  )
  expect_s3_class(p_bar, "ggplot")
  expect_equal(p_bar$labels$fill, "Sex")
  expect_equal(p_bar$theme$legend.direction, "horizontal")
  expect_equal(p_bar$theme$legend.title$size, 9)
  expect_equal(p_bar$theme$legend.text$size, 8)

  p_line <- gg_linechart(
    plot_data,
    year,
    incidence,
    group = sex,
    legend_title = "Sex",
    legend_direction = "horizontal",
    legend_nrow = 1,
    legend_title_size = 9,
    legend_text_size = 8
  )
  expect_s3_class(p_line, "ggplot")
  expect_equal(p_line$labels$colour, "Sex")
  expect_equal(p_line$theme$legend.direction, "horizontal")

  p_pie <- gg_pie(
    plot_data[1:3, ],
    incidence,
    label = cancer,
    legend = TRUE,
    legend_title = "Cancer",
    legend_position = "bottom",
    legend_direction = "horizontal",
    legend_text_size = 8
  )
  expect_s3_class(p_pie, "ggplot")
  expect_equal(p_pie$labels$fill, "Cancer")
  expect_equal(p_pie$theme$legend.position, "bottom")
  expect_equal(p_pie$theme$legend.direction, "horizontal")

  dumbbell_data <- data.frame(
    cancer = rep(c("肺癌", "胃癌", "肝癌"), each = 2),
    sex = rep(c("男", "女"), 3),
    rate = c(55, 38, 28, 18, 31, 12)
  )
  p_dumbbell <- gg_dumbbell(
    dumbbell_data,
    cancer,
    rate,
    sex,
    legend_title = "Sex",
    legend_direction = "horizontal",
    legend_nrow = 1,
    legend_text_size = 8
  )
  expect_s3_class(p_dumbbell, "ggplot")
  expect_equal(p_dumbbell$labels$fill, "Sex")
  expect_equal(p_dumbbell$theme$legend.direction, "horizontal")

  heatmap_data <- data.frame(
    registry = rep(c("A", "B"), each = 2),
    indicator = rep(c("MV", "DCO"), 2),
    value = c(75.2, 2.1, 68.4, 3.5)
  )
  p_heatmap <- gg_heatmap(
    heatmap_data,
    indicator,
    registry,
    value,
    legend_title = "%",
    legend_position = "bottom",
    legend_direction = "horizontal",
    legend_text_size = 8
  )
  expect_s3_class(p_heatmap, "ggplot")
  expect_equal(p_heatmap$labels$fill, "%")
  expect_equal(p_heatmap$theme$legend.position, "bottom")
  expect_equal(p_heatmap$theme$legend.direction, "horizontal")

  expect_error(
    gg_barchart(plot_data, cancer, incidence, group = sex, legend_nrow = 0),
    "legend_nrow"
  )
  expect_error(
    gg_pie(
      plot_data[1:3, ],
      incidence,
      label = cancer,
      legend_direction = "diag"
    ),
    "should be one of"
  )
})

test_that("gg_stack sort_by_y reorders fill segments", {
  stack_data <- data.frame(
    year = rep(2020:2021, each = 3),
    cancer = rep(c("B", "A", "C"), 2),
    cases = c(2, 10, 5, 3, 12, 6)
  )

  # sort_by_y = "none" keeps original order (fill stays as character)
  p_none <- gg_stack(stack_data, year, cases, fill = cancer, sort_by_y = "none")
  fill_col_none <- ggplot2::ggplot_build(p_none)$plot$data$cancer
  expect_true(is.character(fill_col_none) || is.factor(fill_col_none))

  # sort_by_y = "desc" orders by total y descending
  p_desc <- gg_stack(stack_data, year, cases, fill = cancer, sort_by_y = "desc")
  fill_levels_desc <- levels(ggplot2::ggplot_build(p_desc)$plot$data$cancer)
  expect_equal(fill_levels_desc, c("A", "C", "B"))

  # sort_by_y = "asc" orders by total y ascending
  p_asc <- gg_stack(stack_data, year, cases, fill = cancer, sort_by_y = "asc")
  fill_levels_asc <- levels(ggplot2::ggplot_build(p_asc)$plot$data$cancer)
  expect_equal(fill_levels_asc, c("B", "C", "A"))

  # Invalid sort_by_y should error
  expect_error(
    gg_stack(stack_data, year, cases, fill = cancer, sort_by_y = "invalid"),
    "should be one of"
  )
})
