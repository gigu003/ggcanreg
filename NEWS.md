# ggcanreg 0.0.0.9000

## Package infrastructure

* `gg_dumbbell()` now applies `sort` in the visible top-to-bottom order for
  horizontal charts while preserving the corresponding left-to-right order
  for vertical charts.
* `gg_dumbbell()` now gives all difference-label backgrounds a common width
  based on the widest displayed label.
* `gg_dumbbell()` now sorts by the absolute difference when `sort_by = "diff"`.

* `gg_map()` now loads `sf` with the package namespace, so the bundled map
  works when ggcanreg is used directly from an installed package with current
  `dplyr` and `vctrs` versions.
* `gg_map()` now uses `bg_color` instead of `background_color` and supports
  the same `font_family` and `base_size` controls as the other chart helpers.

* `gg_pie()` now uses tighter facet spacing and plot margins for
  `label_position = "inside"`; the x-axis limit also follows the actual pie
  radius so faceted pies use more of the available panel area.

* Compressed lazy-loaded datasets with XZ to reduce installed package size.

* `theme_canreg()` now provides `"report"`, `"publication"`, and `"slides"`
  styles. They respectively provide report hierarchy and light grids, compact
  journal-style axes without background grids, and enlarged presentation text.

* Added a cross-platform GitHub Actions workflow for R CMD check.
* Established a clean package baseline with 227 passing tests and no R CMD
  check errors, warnings, or notes.
* Added a `BugReports` URL to the package metadata.
* Consolidated common internal argument checks so plotting functions provide
  consistent validation while retaining their existing public interfaces.
* `gg_barchart()` now uses a coordinated layout with slimmer bars,
  consistent 12-point typography, restrained grid lines, and improved title,
  legend, axis, and facet spacing. Facet labels remain above the top value-axis
  tick labels. The new `font_family` and `base_size` arguments customize plot
  text and value labels, while `bg_color` controls the complete figure
  background and supports transparent slide overlays.
* `gg_comparison()` now uses the coordinated publication layout and supports
  `font_family`, `base_size`, and `bg_color`; highlighted labels inherit the
  selected font and transparent backgrounds cover both the panel and canvas.
* `gg_dual_barchart()` now uses matched panel dimensions, a clearer center
  gutter, a unified publication-style title hierarchy, and collected grouped
  legends. It also supports `font_family` and `base_size`; the legacy
  `gg_dul_barchart()` alias forwards both arguments. The new `bg_color`
  argument applies to both panels and the complete patchwork canvas. Right-side
  category labels are left-aligned, while vertical category axes are hidden on
  both sides. The two value axes now share a default limit based on the largest
  displayed value, so bar lengths can be compared directly.
* `gg_dumbbell()` now uses publication-style spacing and restrained numeric
  grid lines. It adds title, subtitle, and numeric-axis labels together with
  `font_family`, `base_size`, and complete `bg_color` control.
* `gg_heatmap()` now shares the publication typography and background controls.
  Facet labels are placed above the top axis labels, and tile labels inherit
  `font_family`.
* `gg_linechart()` now uses publication-style typography, compact outer
  margins, restrained horizontal grid lines, and clearer spacing among titles,
  legends, facets, axes, and the plotting panel. The new `font_family` and
  `base_size` arguments also apply to in-panel labels, and `bg_color` controls
  the plot, panel, and facet-strip backgrounds.
* `gg_map()` now draws the bundled `china_map` province boundaries by default,
  supports `map_level` selection, renders auxiliary national boundaries as a
  visible layer, and places the South China Sea region in a framed, enlarged
  inset at the lower right. The islands and dashed boundary are omitted from
  the main panel to keep the national map compact. Islands use dark grey and
  the dashed boundary uses black by default for visibility; `island_color` and
  `auxiliary_color` customize them independently, while `inset_size` controls
  the size of the complete inset. It matches `areacode` directly against
  `china_map$area_code` and resolves the existing area-code workflow locally
  without network access.
  Numeric statistical values are divided into five intervals by default,
  missing regions appear as “No data”, and the legend is shown by default;
  `bins`, `breaks`, `discrete = FALSE`, `legend_nrow`, and `legend_ncol` provide
  explicit control. `legend_spacing` is now independent of legend key
  dimensions; it controls spacing between individual legend keys. Maps hide
  longitude and latitude axes by default. `break_labels` now customizes value
  interval labels. With explicit `breaks` and column-based legends, larger
  values are shown above smaller values. `facet = "province"` or `"city"`
  splits multi-region maps into labeled panels; city faceting defaults to
  city-level background boundaries, and each panel uses its own spatial
  extent. Facet panels use a responsive landscape layout by default;
  `facet_nrow` and `facet_ncol` provide explicit layout control. Custom `sf`
  objects remain supported. Map faceting is implemented with native ggplot2
  facets and returns a regular `ggplot` object. Map output now uses a
  report-ready presentation style with dark-blue left-aligned titles, compact
  centered facet labels, responsive landscape facet layouts, and a bottom
  horizontal legend by default. Automatically generated interval labels use
  spaced ranges such as `116.03 – 211.61` for easier reading. The new
  `footnote` argument adds a left-aligned note below the map and legend.
  The earlier `scale_bar` argument has been removed; add `gg_scale_bar()` to
  the returned plot instead.
  Superseded county codes are matched to current boundaries, and non-finite
  numeric values are shown as missing data rather than stopping map creation.
  The default title area now leaves more space between the subtitle and map.
  When both are supplied, the title and subtitle are kept more closely spaced.
  `bg_color` controls the plot and panel background, including
  transparent output. `facet` now also accepts a data-column name, such as
  `"sex"`, to produce complete maps for each subgroup.
  Map-title spacing is retained when no subtitle is supplied, and title and
  subtitle text no longer overlap on devices rendering large Chinese text.
* Added `gg_north_arrow()` as a reusable, fixed-size north-arrow annotation for
  ggplot maps, with selectable corners, dimensions, padding, line width, and
  label size. It replaces the removed `gg_map(north_arrow = TRUE)` interface.
* Added a bundled nationwide `china_map` `sf` dataset with province-, city-,
  county-level, and auxiliary boundaries for reproducible offline maps.
* `gg_pie()` now supports title and subtitle text plus coordinated
  `font_family`, `base_size`, and `bg_color` controls; center, inside, and
  outside labels all inherit the selected font.
* `gg_pyramid()` now applies the publication hierarchy consistently across its
  paired panels and combined canvas. `font_family` and `base_size` are new,
  while `bg_color` now supports validated transparent export throughout the
  patchwork figure. Age-group labels appear once in the center gutter rather
  than being duplicated along the left edge, and are horizontally centered
  between the two bar panels. Group headings align with the zero edge of their
  respective panels, so the right heading extends rightward with its bars.
  Faceted pyramids now show each facet value at the upper left of its panel.
* Plotting functions now share the `gg_map()` title, subtitle, caption,
  legend, typography, and outer-spacing hierarchy by default. Their coordinated
  default base font size is 12 points.
* `gg_stack()` now supports `font_family`, `base_size`, and `bg_color`, including
  matching fonts for value labels and transparent complete-figure backgrounds.
* Strengthened validation for `gg_pyramid()` and aligned basic `gg_pie()`
  checks with the package's shared internal validation helpers.
* Added earlier, clearer input validation to `gg_dumbbell()` and
  `gg_comparison()` without changing their plotting interfaces.
* Strengthened scalar and data-contract validation for `gg_dual_barchart()`,
  its legacy `gg_dul_barchart()` alias, and `gg_big_value()`.
* Added explicit mapped-variable and finite-value checks to `gg_stack()` and
  `gg_heatmap()`, including safe validation for 100% stacked groups.
* Expanded the README to document existing chart families, formatting
  conventions, backward-compatible aliases, and the offline `sf` map workflow.
* Removed unused internal helpers and stale namespace imports while retaining
  dependencies required by the explicit `read_map()` online helper.
