map_geometry <- sf::st_sfc(
  sf::st_polygon(list(rbind(
    c(0, 0),
    c(1, 0),
    c(1, 1),
    c(0, 1),
    c(0, 0)
  ))),
  sf::st_polygon(list(rbind(
    c(1, 0),
    c(2, 0),
    c(2, 1),
    c(1, 1),
    c(1, 0)
  )))
)

map_data <- sf::st_sf(
  name = c("甲区", "乙区"),
  status = c("达标", "预警"),
  incidence = c(42.1, NA_real_),
  geometry = map_geometry
)

test_that("gg_map uses bundled national boundaries by default", {
  expect_identical(
    "sf" %in% names(getNamespaceImports("ggcanreg")),
    TRUE
  )
  p <- gg_map()
  p_city <- gg_map(facet = "city")
  main_bbox <- sf::st_bbox(p$data)
  frame_bbox <- sf::st_bbox(p$layers[[2]]$data)
  inset_bbox <- sf::st_bbox(p$layers[[3]]$data)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 34L)
  expect_gt(unname(main_bbox[["ymin"]]), 18)
  expect_lt(unname(inset_bbox[["xmin"]]), unname(main_bbox[["xmax"]]))
  expect_equal(
    unname(
      (frame_bbox[["xmin"]] - main_bbox[["xmax"]]) /
        (main_bbox[["xmax"]] - main_bbox[["xmin"]])
    ),
    -0.15,
    tolerance = 1e-6
  )
  expect_setequal(
    c("广东省", "海南省", "台湾省"),
    intersect(
      c("广东省", "海南省", "台湾省"),
      p$layers[[3]]$data$area_name
    )
  )
  expect_identical(p$layers[[2]]$aes_params$colour, "grey60")
  expect_identical(p$layers[[4]]$aes_params$fill, "grey35")
  expect_identical(p$layers[[4]]$aes_params$colour, "grey35")
  expect_identical(p$layers[[5]]$data$area_code, "100000_JD")
  expect_identical(p$layers[[5]]$aes_params$fill, "black")
  expect_identical(p$layers[[5]]$aes_params$colour, "black")
  expect_identical(p$layers[[5]]$aes_params$linewidth, 0.4)
  expect_s3_class(p_city, "ggplot")
  expect_s3_class(p_city$facet, "FacetWrap")
})

test_that("gg_map highlights inset islands and auxiliary boundaries", {
  p <- gg_map(na_value = "pink")
  p_custom <- gg_map(island_color = "red", auxiliary_color = "blue")

  expect_identical(p$layers[[1]]$aes_params$fill, "pink")
  expect_identical(p$layers[[3]]$aes_params$fill, "pink")
  expect_identical(p$layers[[4]]$aes_params$fill, "grey35")
  expect_identical(p$layers[[5]]$aes_params$colour, "black")
  expect_identical(p_custom$layers[[4]]$aes_params$fill, "red")
  expect_identical(p_custom$layers[[5]]$aes_params$colour, "blue")
})

test_that("gg_map controls the South China Sea inset size", {
  p_default <- gg_map()
  p_small <- gg_map(inset_size = 0.75)
  p_large <- gg_map(inset_size = 1.25)
  default_bbox <- sf::st_bbox(p_default$layers[[2]]$data)
  small_bbox <- sf::st_bbox(p_small$layers[[2]]$data)
  large_bbox <- sf::st_bbox(p_large$layers[[2]]$data)
  bbox_width <- function(bbox) unname(bbox[["xmax"]] - bbox[["xmin"]])

  expect_equal(bbox_width(small_bbox) / bbox_width(default_bbox), 0.75)
  expect_equal(bbox_width(large_bbox) / bbox_width(default_bbox), 1.25)
  expect_equal(small_bbox[["xmax"]], default_bbox[["xmax"]])
  expect_equal(large_bbox[["xmax"]], default_bbox[["xmax"]])
  expect_equal(small_bbox[["ymin"]], default_bbox[["ymin"]])
  expect_equal(large_bbox[["ymin"]], default_bbox[["ymin"]])
  expect_snapshot(error = TRUE, gg_map(inset_size = 0))
})

test_that("gg_map keeps titles and subtitles visually compact", {
  p <- gg_map(title = "癌症发病率", subtitle = "河南省，2025 年")

  expect_equal(p$theme$text$size, 12)
  expect_identical(p$theme$text$family, "sans")
  expect_equal(p$theme$plot.title$size, 2)
  expect_equal(p$theme$plot.subtitle$size, 1.15)
  expect_equal(as.numeric(p$theme$plot.title$margin)[3], 4)
  expect_equal(as.numeric(p$theme$plot.subtitle$margin)[3], 14)
  expect_equal(as.numeric(p$theme$plot.margin), c(14, 18, 12, 18))
})

test_that("gg_map selects bundled boundary levels", {
  p_city <- gg_map(map_level = "city")
  p_county <- gg_map(map_level = "county")

  expect_equal(nrow(p_city$data), 363L)
  expect_equal(nrow(p_county$data), 2840L)
  expect_gt(unname(sf::st_bbox(p_city$data)[["ymin"]]), 18)
  expect_gt(unname(sf::st_bbox(p_county$data)[["ymin"]]), 18)
  expect_snapshot(error = TRUE, gg_map(map_level = "village"))
})

test_that("gg_map matches areacode directly to bundled china_map area codes", {
  p <- gg_map(areacode = c("410102", "410103"))

  expect_equal(nrow(p$data), 2L)
  expect_setequal(p$data$area_code, c("410102", "410103"))
  expect_equal(length(p$layers), 1L)

  expect_message(
    partial <- gg_map(areacode = c("410102", "999999")),
    "999999"
  )
  expect_identical(partial$data$area_code, "410102")
  expect_snapshot(error = TRUE, gg_map(areacode = "999999"))
})

test_that("gg_map joins data frames to bundled boundaries without network", {
  registry_data <- data.frame(
    code = c("410102", "410103"),
    incidence = c(42.1, 38.7)
  )

  p <- gg_map(
    registry_data,
    areacode = code,
    value = incidence,
    bg_level = "province",
    bg_bound = "county"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(any(p$layers[[1]]$data$adcode == "410102"), TRUE)
  expect_equal(length(p$layers), 2L)
})

test_that("gg_map matches legacy county codes and non-finite value columns", {
  asr_data <- data.frame(
    areacode = c("410123", "411023", "410102"),
    asr_cn2000 = c(120, 180, Inf)
  )

  expect_snapshot(
    p <- gg_map(
      asr_data,
      areacode = "areacode",
      value = "asr_cn2000",
      bg_level = "province"
    )
  )
  expect_identical(
    p$layers[[2]]$data$adcode[p$layers[[2]]$data$adcode == "410181"],
    "410181"
  )
  expect_identical(
    p$layers[[2]]$data$adcode[p$layers[[2]]$data$adcode == "411003"],
    "411003"
  )
  expect_equal(
    as.character(p$layers[[2]]$data$.ggcanreg_fill[
      p$layers[[2]]$data$adcode == "410102"
    ]),
    "No data"
  )
})

test_that("gg_map accepts quoted data frame column names", {
  cr <- data.frame(
    areacode = c("410102", "410103"),
    cr = c(42.1, 38.7)
  )

  p <- gg_map(cr, areacode = "areacode", value = "cr")

  expect_s3_class(p, "ggplot")
  expect_equal(any(p$layers[[1]]$data$adcode == "410102"), TRUE)
  expect_identical(
    rlang::as_label(p$layers[[2]]$mapping$fill),
    ".ggcanreg_fill"
  )
  expect_equal(
    p$layers[[2]]$data$cr[p$layers[[2]]$data$adcode == "410102"],
    42.1
  )
  expect_equal(is.factor(p$layers[[2]]$data$.ggcanreg_fill), TRUE)
  expect_s3_class(p$theme$axis.text, "element_blank")
  expect_s3_class(p$theme$axis.ticks, "element_blank")
})

test_that("gg_map displays an optional footnote below the map", {
  p <- gg_map(
    map_data,
    fill = incidence,
    footnote = "数据来源：河南省肿瘤登记年报"
  )

  expect_identical(p$labels$caption, "数据来源：河南省肿瘤登记年报")
  expect_identical(p$theme$plot.caption.position, "plot")
  expect_identical(p$theme$plot.caption$hjust, 0)
  expect_snapshot(
    error = TRUE,
    gg_map(map_data, fill = incidence, footnote = 1)
  )
})

test_that("gg_map supports colored and transparent backgrounds", {
  p <- gg_map(
    map_data,
    fill = incidence,
    bg_color = "transparent"
  )

  expect_identical(p$theme$plot.background$fill, "transparent")
  expect_identical(p$theme$panel.background$fill, "transparent")
  expect_snapshot(
    error = TRUE,
    gg_map(map_data, fill = incidence, bg_color = 1)
  )
})

test_that("gg_map uses the coordinated typography controls", {
  p <- gg_map(
    map_data,
    fill = incidence,
    label = name,
    font_family = "serif",
    base_size = 14
  )

  expect_identical(p$theme$text$family, "serif")
  expect_equal(p$theme$text$size, 14)
  expect_identical(p$layers[[2]]$aes_params$family, "serif")
  expect_identical("background_color" %in% names(formals(gg_map)), FALSE)
})

test_that("gg_map leaves map decorations to standalone layers", {
  expect_identical("north_arrow" %in% names(formals(gg_map)), FALSE)
  expect_identical("scale_bar" %in% names(formals(gg_map)), FALSE)
})

test_that("gg_map retains directly administered requested boundaries", {
  registry_data <- data.frame(code = "419001", incidence = 42.1)

  p <- gg_map(registry_data, areacode = code, value = incidence)

  expect_equal(any(p$layers[[1]]$data$adcode == "419001"), TRUE)
})

test_that("gg_map facets bundled boundaries by province or city", {
  regional_data <- data.frame(
    areacode = c("110101", "410102", "410103"),
    incidence = c(10, 20, 30)
  )

  p_province <- gg_map(
    regional_data,
    areacode = areacode,
    value = incidence,
    facet = "province"
  )
  p_city <- gg_map(
    regional_data,
    areacode = areacode,
    value = incidence,
    facet = "city"
  )

  expect_s3_class(p_province, "ggplot")
  expect_s3_class(p_city, "ggplot")
  expect_s3_class(p_city$facet, "FacetWrap")
  expect_equal(as.numeric(p_city$theme$strip.text$size), 0.85)
  expect_identical(p_city$theme$strip.text$colour, "#0B2D63")
  expect_equal(
    nlevels(p_province$layers[[1]]$data$.ggcanreg_facet),
    2L
  )
  expect_equal(
    nlevels(p_city$layers[[1]]$data$.ggcanreg_facet),
    2L
  )
  city_bboxes <- lapply(
    split(
      p_city$layers[[1]]$data,
      p_city$layers[[1]]$data$.ggcanreg_facet
    ),
    sf::st_bbox
  )
  city_dimensions <- vapply(
    city_bboxes,
    function(bbox) {
      max(
        bbox[["xmax"]] - bbox[["xmin"]],
        bbox[["ymax"]] - bbox[["ymin"]]
      )
    },
    numeric(1)
  )
  expect_equal(unname(city_dimensions), rep(1, length(city_dimensions)))
})

test_that("gg_map facets a complete map by a data column", {
  sex_data <- expand.grid(
    areacode = c("410102", "410103", "410104"),
    sex = c("Male", "Female"),
    stringsAsFactors = FALSE
  )
  sex_data$incidence <- seq(100, 200, length.out = nrow(sex_data))
  p <- gg_map(
    sex_data,
    areacode = "areacode",
    value = "incidence",
    facet = "sex",
    bg_level = "province"
  )

  expect_s3_class(p$facet, "FacetWrap")
  expect_identical(
    levels(p$layers[[1]]$data$.ggcanreg_facet),
    c("Male", "Female")
  )
  expect_equal(
    nrow(p$layers[[1]]$data),
    2L *
      nrow(p$layers[[1]]$data[p$layers[[1]]$data$.ggcanreg_facet == "Male", ])
  )
  expect_snapshot(
    error = TRUE,
    gg_map(
      sex_data,
      areacode = "areacode",
      value = "incidence",
      facet = "not_a_column"
    )
  )
})

test_that("facet maps use a landscape presentation layout", {
  henan <- ggcanreg::china_map |>
    dplyr::filter(
      .data$province_code == "410000",
      .data$area_level == "city"
    )
  p <- gg_map(henan, facet = "city")

  expect_identical(p$facet$params$ncol, 6L)
  expect_equal(as.numeric(p$theme$plot.title$size), 2)
  expect_identical(p$theme$plot.title$colour, "#0B2D63")
  expect_equal(as.numeric(p$theme$plot.title$margin)[3], 20)
  expect_identical(p$theme$plot.subtitle$colour, "#40577F")
  expect_equal(as.numeric(p$theme$plot.subtitle$margin)[3], 20)
  expect_equal(
    as.numeric(p$theme$panel.spacing.x, unitTo = "lines"),
    0.35
  )
  expect_equal(
    as.numeric(p$theme$panel.spacing.y, unitTo = "lines"),
    0.55
  )
})

test_that("native sf facets preserve geometry aspect ratios", {
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0),
      c(10, 0),
      c(10, 1),
      c(0, 1),
      c(0, 0)
    ))),
    sf::st_polygon(list(rbind(
      c(100, 0),
      c(101, 0),
      c(101, 10),
      c(100, 10),
      c(100, 0)
    )))
  )
  facet_data <- sf::st_sf(
    city_code = c("wide", "tall"),
    city_name = c("Wide", "Tall"),
    incidence = c(10, 20),
    geometry = geometry
  )

  p <- gg_map(facet_data, fill = incidence, facet = "city")
  facet_bboxes <- lapply(split(p$data, p$data$.ggcanreg_facet), sf::st_bbox)
  aspect_ratios <- vapply(
    facet_bboxes,
    function(bbox) {
      (bbox[["xmax"]] - bbox[["xmin"]]) /
        (bbox[["ymax"]] - bbox[["ymin"]])
    },
    numeric(1)
  )

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(unname(aspect_ratios), c(10, 0.1))
})

test_that("facet maps apply legend position to the ggplot", {
  regional_data <- data.frame(
    areacode = c("110101", "410102", "410103"),
    incidence = c(10, 20, 30)
  )
  p <- gg_map(
    regional_data,
    areacode = areacode,
    value = incidence,
    facet = "city",
    legend_position = "bottom"
  )

  expect_s3_class(p, "ggplot")
  expect_identical(p$theme$legend.position, "bottom")
})

test_that("facet maps support explicit row and column layouts", {
  regional_data <- data.frame(
    areacode = c("110101", "410102", "410103"),
    incidence = c(10, 20, 30)
  )
  p_ncol <- gg_map(
    regional_data,
    areacode = areacode,
    value = incidence,
    facet = "city",
    facet_ncol = 1
  )
  p_nrow <- gg_map(
    regional_data,
    areacode = areacode,
    value = incidence,
    facet = "city",
    facet_nrow = 1
  )

  expect_identical(p_ncol$facet$params$ncol, 1L)
  expect_identical(p_nrow$facet$params$nrow, 1L)
  expect_snapshot(
    gg_map(
      regional_data,
      areacode = areacode,
      value = incidence,
      facet = "city",
      facet_ncol = 1.5
    ),
    error = TRUE
  )
})

test_that("gg_map bins numeric sf data and shows missing data by default", {
  p <- gg_map(
    map_data,
    fill = incidence,
    label = name,
    na_value = "grey90"
  )
  built <- ggplot2::ggplot_build(p)
  fill_scale <- built$plot$scales$get_scales("fill")

  expect_s3_class(p, "ggplot")
  expect_s3_class(fill_scale, "ScaleDiscrete")
  expect_s3_class(p$theme$axis.text, "element_blank")
  expect_identical(p$theme$legend.position, "bottom")
  expect_equal(p$labels$fill, "Incidence")
  expect_identical(
    p$guides$guides$fill$params$theme$legend.title.position,
    "left"
  )
  expect_equal(built$data[[1]]$fill[2], "grey90")
  expect_equal(tail(fill_scale$get_breaks(), 1L), "No data")
  expect_equal(tail(fill_scale$get_labels(), 1L), "No data")
})

test_that("gg_map creates five intervals and honors explicit breaks", {
  numeric_map <- map_data[rep(1L, 6L), ]
  numeric_map$incidence <- c(0, 10, 20, 30, 40, NA_real_)

  p_default <- gg_map(numeric_map, fill = incidence)
  p_breaks <- gg_map(numeric_map, fill = incidence, breaks = c(0, 20, 40))
  p_labels <- gg_map(
    numeric_map,
    fill = incidence,
    breaks = c(0, 20, 40),
    break_labels = c("低", "高")
  )
  p_columns <- gg_map(
    numeric_map,
    fill = incidence,
    breaks = c(0, 20, 40),
    legend_ncol = 2
  )
  p_continuous <- gg_map(numeric_map, fill = incidence, discrete = FALSE)
  p_labels_built <- ggplot2::ggplot_build(p_labels)
  p_columns_built <- ggplot2::ggplot_build(p_columns)

  expect_equal(nlevels(p_default$data$.ggcanreg_fill), 6L)
  expect_equal(tail(levels(p_default$data$.ggcanreg_fill), 1L), "No data")
  expect_equal(
    levels(p_breaks$data$.ggcanreg_fill),
    c("[0,20]", "(20,40]", "No data")
  )
  expect_equal(
    ggplot2::ggplot_build(p_breaks)$plot$scales$get_scales("fill")$get_labels(),
    c("0 \u2013 20", "20 \u2013 40", "No data")
  )
  expect_equal(
    ggplot2::ggplot_build(p_breaks)$plot$scales$get_scales("fill")$get_labels(),
    c("0 – 20", "20 – 40", "No data")
  )
  expect_equal(
    p_labels_built$plot$scales$get_scales("fill")$get_labels(),
    c("低", "高", "No data")
  )
  expect_identical(p_columns$guides$guides$fill$params$reverse, TRUE)
  expect_identical(
    tail(p_columns_built$plot$scales$get_scales("fill")$get_breaks(), 1L),
    "(20,40]"
  )
  expect_identical(
    head(p_columns_built$plot$scales$get_scales("fill")$get_breaks(), 1L),
    "No data"
  )
  expect_s3_class(
    p_continuous$scales$get_scales("fill"),
    "ScaleContinuous"
  )
  expect_identical(
    p_default$guides$guides$fill$params$theme$legend.title.position,
    "left"
  )
})

test_that("gg_map controls discrete legend rows and columns", {
  p_nrow <- gg_map(map_data, fill = status, legend_nrow = 1)
  p_ncol <- gg_map(map_data, fill = status, legend_ncol = 1)

  guide_nrow <- p_nrow$guides$guides$fill
  guide_ncol <- p_ncol$guides$guides$fill

  expect_equal(guide_nrow$params$nrow, 1)
  expect_equal(guide_ncol$params$ncol, 1)
})

test_that("gg_map legend spacing does not set key dimensions", {
  p_small <- gg_map(
    map_data,
    fill = status,
    legend_spacing = 0.1
  )
  p_large <- gg_map(
    map_data,
    fill = status,
    legend_spacing = 1
  )

  expect_null(p_small$theme$legend.key.width)
  expect_null(p_small$theme$legend.key.height)
  expect_null(p_small$theme$legend.key.spacing)
  expect_equal(p_small$theme$legend.key.spacing.x, grid::unit(0.1, "cm"))
  expect_equal(p_small$theme$legend.key.spacing.y, grid::unit(0.1, "cm"))
  expect_equal(p_large$theme$legend.key.spacing.x, grid::unit(1, "cm"))
  expect_equal(p_large$theme$legend.key.spacing.y, grid::unit(1, "cm"))
  expect_identical(
    p_small$theme$legend.key.width,
    p_large$theme$legend.key.width
  )
  expect_identical(
    p_small$theme$legend.key.height,
    p_large$theme$legend.key.height
  )
})

test_that("gg_map supports discrete and positional sf fill variables", {
  p_discrete <- gg_map(
    map_data,
    fill = status,
    palette = "quality",
    legend = TRUE
  )
  p_positional <- gg_map(map_data, incidence)
  p_quoted <- gg_map(map_data, fill = "incidence")

  expect_s3_class(p_discrete, "ggplot")
  expect_s3_class(p_positional, "ggplot")
  expect_identical(
    rlang::as_label(p_quoted$layers[[1]]$mapping$fill),
    ".ggcanreg_fill"
  )
})

test_that("gg_map validates sf labels clearly", {
  no_name <- map_data[, c("incidence")]

  expect_snapshot(error = TRUE, gg_map(no_name, fill = incidence, label = TRUE))
  expect_snapshot(
    error = TRUE,
    gg_map(map_data, fill = incidence, label = missing)
  )
})

test_that("gg_map validates sf styling arguments", {
  expect_snapshot(error = TRUE, gg_map(map_data, border_width = -0.1))
})
