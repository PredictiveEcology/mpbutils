utils::globalVariables(c("."))

#' Create study area based o ecoregion selection
#'
#' @param ecoregions numeric vector indicating which ecoregions to be included as part of the study
#'                   area. Derived from \url{http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip}.
#' @param targetCRS target CRS string to use for reprojecting ecodistrict (study area) polygons.
#' @param cPath cache path
#' @param dPath destination path
#'
#' @return an `sf` object
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom reproducible prepInputs
#' @importFrom sf st_read st_transform
mpbStudyArea <- function(ecoregions = c(112, 120, 122, 124, 126), targetCRS, cPath, dPath) {
  shp <- prepInputs(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                    targetFile = "ecoregions.shp", alsoExtract = "similar",
                    fun = "sf::st_read",
                    cacheRepo = cPath,
                    destinationPath = dPath) %>%
    st_transform(., targetCRS) ## keep as sf for plotting

  shp[shp$REGION_ID %in% ecoregions, ]
}
