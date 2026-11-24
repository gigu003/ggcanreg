test_that("china_map contains unified nationwide boundaries", {
  data_environment <- new.env(parent = emptyenv())
  utils::data("china_map", package = "ggcanreg", envir = data_environment)
  map <- data_environment$china_map

  expect_s3_class(map, "sf")
  expect_identical(sf::st_crs(map)$epsg, 4326L)
  expect_equal(nrow(map), 3238L)
  expect_equal(
    as.vector(table(factor(
      map$area_level,
      levels = c("province", "city", "county", "auxiliary")
    ))),
    c(34L, 363L, 2840L, 1L)
  )
  expect_equal(anyDuplicated(map$area_code[map$area_level != "auxiliary"]), 0L)
  expect_equal(any(sf::st_is_empty(map)), FALSE)
  expect_equal(all(sf::st_is_valid(sf::st_transform(map, 3857))), TRUE)
})
