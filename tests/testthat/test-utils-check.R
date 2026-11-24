test_that("shared checks accept valid values", {
  expect_invisible(ggcanreg:::.check_data_frame(data.frame(x = 1)))
  expect_invisible(ggcanreg:::.check_single_logical(TRUE, "flag"))
  expect_invisible(ggcanreg:::.check_single_character("value", "text"))
  expect_invisible(ggcanreg:::.check_single_number(1, "size", min = 0))
  expect_invisible(ggcanreg:::.check_digits(0))
})

test_that("shared checks report the argument name", {
  expect_snapshot(error = TRUE, ggcanreg:::.check_data_frame(1))
  expect_snapshot(error = TRUE, ggcanreg:::.check_single_logical(NA, "flag"))
  expect_snapshot(error = TRUE, ggcanreg:::.check_single_character(1, "text"))
  expect_snapshot(
    error = TRUE,
    ggcanreg:::.check_single_number(0, "size", min = 0)
  )
  expect_snapshot(error = TRUE, ggcanreg:::.check_digits(-1, "label_digits"))
})
