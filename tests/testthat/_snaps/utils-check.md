# shared checks report the argument name

    Code
      ggcanreg:::.check_data_frame(1)
    Condition
      Error:
      ! `data` must be a data frame.

---

    Code
      ggcanreg:::.check_single_logical(NA, "flag")
    Condition
      Error:
      ! `flag` must be TRUE or FALSE.

---

    Code
      ggcanreg:::.check_single_character(1, "text")
    Condition
      Error:
      ! `text` must be a single character value.

---

    Code
      ggcanreg:::.check_single_number(0, "size", min = 0)
    Condition
      Error:
      ! `size` must be greater than 0.

---

    Code
      ggcanreg:::.check_digits(-1, "label_digits")
    Condition
      Error:
      ! `label_digits` must be a single non-negative number.

