# line and bar charts support complete figure backgrounds

    Code
      gg_linechart(plot_data, year, incidence, bg_color = 1)
    Condition
      Error:
      ! `bg_color` must be a single character value.

---

    Code
      gg_barchart(plot_data, cancer, incidence, bg_color = "not-a-color")
    Condition
      Error:
      ! `bg_color` must be a valid R color.

