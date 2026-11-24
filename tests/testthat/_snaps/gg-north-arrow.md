# gg_north_arrow validates its arguments

    Code
      gg_north_arrow(location = "center")
    Condition
      Error:
      ! `location` must be one of "tl", "tr", "bl", or "br".

---

    Code
      gg_north_arrow(width = 0)
    Condition
      Error:
      ! `width` must be a single positive finite number.

---

    Code
      gg_north_arrow(pad_x = -0.1)
    Condition
      Error:
      ! `pad_x` must be a single non-negative finite number.

---

    Code
      gg_north_arrow(text_size = Inf)
    Condition
      Error:
      ! `text_size` must be a single positive finite number.

