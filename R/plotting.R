#' @export
plot_StudyAreaFn <- function(absk, cols, studyArea, mam, pred, propPineMap,
                             thresholdAttackTreesMinDetectable, thresholdPineProportion = NULL) {
  mam[mam[] == 0] <- NA
  absksp <- sf::as_Spatial(absk)

  # pred <- cleanUpPredictionRas(pred)
  pred[pred[] < log(thresholdAttackTreesMinDetectable * 12)] <- NA # 12 years ... hard coded, yikes! # TODO
  # # ratioLog <- exp(mean(log(pred[]), na.rm = TRUE)/mean(log(mam[]), na.rm = TRUE))
  # # ratio <- mean(pred[], na.rm = TRUE)/mean(mam[], na.rm = TRUE)
  # # pred[] <- pred[] # / ratio
  if (!is.null( thresholdPineProportion)) {
    thresholdPineProportion <- 0.4
    makeNA <- propPineMap[] <= 0
    propPineMap[makeNA] <- NA
    makeNA <- propPineMap[] <= thresholdPineProportion | makeNA
    pred[makeNA] <- NA
    pred[] <- pred[]*propPineMap[]
  }
  viewMode <- suppressMessages(identical(tmap_mode(), "view"))
  tmap_style("white")
  t1 <- tm_shape(absk) + tm_polygons(alpha = 0) + tm_graticules(alpha = 0.2) #tm_grid(projection = st_crs(4326), alpha = 0.15)
  t1 <- t1 + tm_shape(studyArea) + tm_polygons(alpha = 0, border.col = "lightblue")
  if (viewMode)
    t1 <- tm_basemap("OpenStreetMap") + t1 # only with tmap_mode("view")
  #t5 <- tm_tiles("OpenStreetMap")
  brks <- c(0, 2, 4, 6, 8, 13)
  t2 <- tm_shape(mam) + tm_raster(title = "Accumulated Damage", alpha = 0.8, palette = "YlOrRd",
                                  style = "fixed", breaks = brks, legend.show = FALSE)
  t3 <- tm_shape(propPineMap) + tm_raster(title = "Pine Cover (prop)", alpha = 0.6, palette = "Greens")
  t3b <- t3 + tm_legend(show = FALSE)
  t4 <- tm_shape(pred) + tm_raster(title = "log(Attacked trees)", alpha = 0.8, palette = "YlOrRd", style = "fixed", breaks = brks)
  tpred <- t3 + t4 + t1 + tm_legend(show = TRUE, position = c("right", "top"))
  if (viewMode) {
    tpred
  } else {
    tpred <- tpred + tm_layout(title = "Predicted")
    tmam <- t3b + t2 + t1 + tm_compass(type  = "4star", position = c("right", "top"), north = 10) + tm_layout(title = "Observed")
    tmap_arrange(tmam, tpred)
  }
}


#' @export
plot_CentroidShift <- function(centroids, title) {
  ggplot(centroids[], aes(x = layerName, y = value, group = xy, color = type)) +
    geom_point() +
    facet_grid(vars(xy), scales = "free_y") +
    ggplot2::theme_bw() +
    ggtitle(title)
}

#' @export
plot_HistsOfDEoptimPars <- function(fit_mpbSpreadOptimizer, title) {
  (paramHists <- fit_mpbSpreadOptimizer %>%
     ggplot( aes(x=value, fill=Parameter)) +
     geom_histogram() +# color="#e9ecef", alpha=0.6, position = 'identity') +
     facet_grid(. ~ Parameter, scales = "free") +
     theme_bw() +
     ggtitle(title))
}


#' @export
plot_ObsVPredAbund1 <- function(dt) {
  ggplot(dt, aes(x = `Observed Mean`, y = `Predicted Mean`)) +
    geom_point() +
    theme_bw() +
    xlab("Observed mean attack density, per year") +
    ylab("Predicted mean attack density, per year")
}

#' @export
plot_ObsVPredAbund2 <- function(dt) {
  dtL <- melt(dt, id.vars = "Year")
  ggplot(dtL, aes(x = Year, y = value, group = variable, col = variable)) +
    geom_line() +
    theme_bw() +
    scale_y_continuous(trans='log') +
    xlab("Year") +
    ylab("Predicted and Observed mean attack density, per year")
}
