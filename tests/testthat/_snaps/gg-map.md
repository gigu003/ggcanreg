# gg_map controls the South China Sea inset size

    Code
      gg_map(inset_size = 0)
    Condition
      Error:
      ! `inset_size` must be a single positive finite number.

# gg_map selects bundled boundary levels

    Code
      gg_map(map_level = "village")
    Condition
      Error:
      ! `map_level` must be one of "province", "city", "county".

# gg_map matches areacode directly to bundled china_map area codes

    Code
      gg_map(areacode = "999999")
    Condition
      Error:
      ! No `china_map$area_code` values matched `areacode`: 999999.

# gg_map matches legacy county codes and non-finite value columns

    Code
      p <- gg_map(asr_data, areacode = "areacode", value = "asr_cn2000", bg_level = "province")
    Condition
      Warning:
      Non-finite map fill values were treated as missing.

# gg_map displays an optional footnote below the map

    Code
      gg_map(map_data, fill = incidence, footnote = 1)
    Condition
      Error:
      ! `footnote` must be a single character value.

# gg_map supports colored and transparent backgrounds

    Code
      gg_map(map_data, fill = incidence, bg_color = 1)
    Condition
      Error:
      ! `bg_color` must be a single character value.

# gg_map facets a complete map by a data column

    Code
      gg_map(sex_data, areacode = "areacode", value = "incidence", facet = "not_a_column")
    Condition
      Error:
      ! `facet = "not_a_column"` does not name a column in `data`.

# facet maps support explicit row and column layouts

    Code
      gg_map(regional_data, areacode = areacode, value = incidence, facet = "city",
        facet_ncol = 1.5)
    Condition
      Error:
      ! `facet_ncol` must be NULL or a single positive integer.

# gg_map validates sf labels clearly

    Code
      gg_map(no_name, fill = incidence, label = TRUE)
    Condition
      Error:
      ! `label = TRUE` requires a `name` or `area_name` column in the `sf` object.

---

    Code
      gg_map(map_data, fill = incidence, label = missing)
    Condition
      Error:
      ! The custom `label` variable `missing` was not found in `data`.

# gg_map validates sf styling arguments

    Code
      gg_map(map_data, border_width = -0.1)
    Condition
      Error:
      ! `border_width` must be a single non-negative number.

