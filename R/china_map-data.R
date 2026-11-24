#' China administrative boundaries
#'
#' Simplified province-, city-, and county-level administrative boundaries for
#' China, stored together as WGS 84 geometries. Use `area_level` to select the
#' required administrative level. The object also retains the auxiliary
#' national boundary feature supplied by the source.
#'
#' @format An `sf` object with 3,238 rows and 11 variables:
#' \describe{
#'   \item{area_code}{Six-digit administrative code, or the source identifier
#'     for an auxiliary boundary.}
#'   \item{area_name}{Administrative area name.}
#'   \item{area_level}{Boundary level: `"province"`, `"city"`, `"county"`, or
#'     `"auxiliary"`.}
#'   \item{parent_code}{Code of the parent administrative area.}
#'   \item{province_code}{Province-level administrative code.}
#'   \item{province_name}{Province-level administrative area name.}
#'   \item{children_number}{Number of child areas reported by the source.}
#'   \item{source}{Boundary data source.}
#'   \item{source_url}{Base URL of the boundary data source.}
#'   \item{retrieved_on}{Date on which the source snapshot was retrieved.}
#'   \item{geometry}{Boundary geometry.}
#' }
#' @details
#' The geometries were simplified with a 500-metre tolerance. The source
#' provides 34 province-level, 363 city-level, 2,840 county-level, and one
#' auxiliary boundary feature in this snapshot. Source coverage and area names
#' reflect the data available on `retrieved_on` and should not be interpreted as
#' an independently verified official administrative division register.
#' @source DataV.GeoAtlas,
#'   \url{https://geo.datav.aliyun.com/areas_v3/bound/}
#' @examples
#' province_boundaries <- china_map[china_map$area_level == "province", ]
#' city_boundaries <- china_map[china_map$area_level == "city", ]
#' county_boundaries <- china_map[china_map$area_level == "county", ]
"china_map"
