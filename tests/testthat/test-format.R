test_that("format_rate formats numeric rates", {
  expect_equal(format_rate(12.345, digits = 1), "12.3")
  expect_equal(format_rate(12.346, digits = 2), "12.35")
})

test_that("format_percent supports percentage and proportion scales", {
  expect_equal(format_percent(23.46, digits = 1), "23.5%")
  expect_equal(format_percent(0.2345, digits = 1, scale = 100), "23.4%")
})

test_that("format_count formats counts with comma grouping", {
  expect_equal(format_count(1234), "1,234")
})

test_that("format helpers validate inputs", {
  expect_error(format_rate(1, digits = -1), "digits")
  expect_error(format_percent(1, scale = NA_real_), "scale")
})
