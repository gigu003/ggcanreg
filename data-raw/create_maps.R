## Code to prepare the nationwide administrative boundary dataset.

library(curl)
library(dplyr)
library(sf)
library(stringr)
library(usethis)

# ============================================================
# 1. 基本参数
# ============================================================

source_base_url <- "https://geo.datav.aliyun.com/areas_v3/bound"
crs_storage <- 4326
simplify_tolerance <- 500
retrieved_on <- Sys.Date()

cache_dir <- file.path(
  tools::R_user_dir("ggcanreg", which = "cache"),
  "china-map-datav"
)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. 下载和读取函数
# ============================================================

boundary_url <- function(area_code) {
  paste0(source_base_url, "/", area_code, "_full.json")
}

boundary_cache_path <- function(area_code) {
  file.path(cache_dir, paste0(area_code, "_full.json"))
}

download_boundaries <- function(area_codes, batch_size = 20L) {
  area_codes <- unique(as.character(area_codes))
  destination <- vapply(area_codes, boundary_cache_path, character(1))
  needs_download <- !file.exists(destination) |
    is.na(file.info(destination)$size) |
    file.info(destination)$size == 0L

  if (!any(needs_download)) {
    return(invisible(destination))
  }

  download_codes <- area_codes[needs_download]
  download_paths <- destination[needs_download]
  batches <- split(
    seq_along(download_codes),
    ceiling(seq_along(download_codes) / batch_size)
  )

  for (batch in batches) {
    result <- curl::multi_download(
      urls = vapply(download_codes[batch], boundary_url, character(1)),
      destfiles = download_paths[batch],
      resume = TRUE,
      progress = interactive(),
      multi_timeout = 300
    )

    failed <- !result$success | result$status_code != 200L
    if (any(failed)) {
      stop(
        paste0(
          "Failed to download administrative boundaries: ",
          paste(download_codes[batch][failed], collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }
  }

  invisible(destination)
}

extract_parent_code <- function(x) {
  str_extract(as.character(x), "[0-9]{6}")
}

read_boundary_file <- function(area_code) {
  boundary <- st_read(
    boundary_cache_path(area_code),
    quiet = TRUE,
    stringsAsFactors = FALSE
  )

  if (!"parent" %in% names(boundary)) {
    boundary$parent <- NA_character_
  }

  boundary |>
    transmute(
      area_code = as.character(adcode),
      area_name = as.character(name),
      source_level = as.character(level),
      parent_code = extract_parent_code(parent),
      children_number = as.integer(childrenNum),
      geometry
    )
}

read_boundaries <- function(area_codes) {
  download_boundaries(area_codes)
  bind_rows(lapply(area_codes, read_boundary_file))
}

simplify_boundaries <- function(x, tolerance = simplify_tolerance) {
  x |>
    st_transform(3857) |>
    st_make_valid() |>
    st_simplify(dTolerance = tolerance, preserveTopology = TRUE) |>
    st_make_valid() |>
    st_transform(crs_storage)
}

# ============================================================
# 3. 获取省级边界
# ============================================================

province_raw <- read_boundaries("100000")

province <- province_raw |>
  filter(str_detect(area_code, "^[0-9]{6}$"), source_level == "province")

auxiliary_boundary <- province_raw |>
  filter(!str_detect(area_code, "^[0-9]{6}$") | is.na(source_level))

if (nrow(province) == 0L) {
  stop("The national boundary file contains no provinces.", call. = FALSE)
}

# ============================================================
# 4. 获取省级以下第一层边界
#
# 直辖市下辖区以及省直管县级行政区会直接出现在这一层。
# ============================================================

first_level_parent_codes <- province |>
  st_drop_geometry() |>
  filter(children_number > 0L) |>
  pull(area_code)

first_level <- read_boundaries(first_level_parent_codes)

# ============================================================
# 5. 获取第二层边界
#
# 仅对仍有下级边界的第一层行政区下载 `_full.json`。
# ============================================================

second_level_parent_codes <- first_level |>
  st_drop_geometry() |>
  filter(children_number > 0L) |>
  pull(area_code) |>
  unique()

second_level <- read_boundaries(second_level_parent_codes)

# ============================================================
# 6. 合并全国边界并统一字段
# ============================================================

province_lookup <- province |>
  st_drop_geometry() |>
  select(province_code = area_code, province_name = area_name)

china_map <- bind_rows(
  province,
  first_level,
  second_level,
  auxiliary_boundary
) |>
  mutate(
    area_level = case_when(
      source_level == "province" ~ "province",
      source_level == "city" ~ "city",
      source_level == "district" ~ "county",
      TRUE ~ "auxiliary"
    ),
    province_code = if_else(
      area_level == "auxiliary",
      NA_character_,
      paste0(str_sub(area_code, 1L, 2L), "0000")
    )
  ) |>
  left_join(province_lookup, by = "province_code") |>
  mutate(
    source = "DataV.GeoAtlas",
    source_url = source_base_url,
    retrieved_on = retrieved_on
  ) |>
  select(
    area_code,
    area_name,
    area_level,
    parent_code,
    province_code,
    province_name,
    children_number,
    source,
    source_url,
    retrieved_on,
    geometry
  ) |>
  arrange(
    match(area_level, c("province", "city", "county", "auxiliary")),
    area_code
  ) |>
  simplify_boundaries()

# ============================================================
# 7. 数据检查
# ============================================================

administrative_map <- china_map |>
  filter(area_level != "auxiliary")

duplicate_codes <- administrative_map$area_code[
  duplicated(administrative_map$area_code)
]

if (length(duplicate_codes) > 0L) {
  stop(
    paste0(
      "Duplicate administrative codes found: ",
      paste(unique(duplicate_codes), collapse = ", "),
      "."
    ),
    call. = FALSE
  )
}

invalid_geometry <- which(!st_is_valid(st_transform(china_map, 3857)))
if (length(invalid_geometry) > 0L) {
  stop(
    paste0(
      "Invalid geometries found after simplification: ",
      paste(china_map$area_code[invalid_geometry], collapse = ", "),
      "."
    ),
    call. = FALSE
  )
}

stopifnot(
  sum(china_map$area_level == "province") == 34L,
  sum(china_map$area_level == "city") > 300L,
  sum(china_map$area_level == "county") > 2500L,
  all(!is.na(administrative_map$area_code)),
  all(!is.na(administrative_map$area_name)),
  all(!is.na(administrative_map$province_code)),
  all(!is.na(administrative_map$province_name)),
  !any(st_is_empty(china_map)),
  st_crs(china_map)$epsg == crs_storage
)

print(count(st_drop_geometry(china_map), area_level))
print(china_map)

# ============================================================
# 8. 保存为单一包内数据
# ============================================================

usethis::use_data(china_map, overwrite = TRUE, compress = "xz")
