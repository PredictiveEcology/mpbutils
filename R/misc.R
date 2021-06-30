#' Calculate cumulative sum of a stack, with optional log
#'
#' @param stk a RasterStack
#' @param log Logical. Will take \code{log(map) + 1}
#' @export
cumulativeMap <- function(stk, log = TRUE) {
  for (lay in names(stk)) {
    stk[[lay]][is.na(stk[[lay]][])] <- 0
  }
  out <- if (nlayers(stk) > 1) sum(stk) else stk[[1]]
  if (isTRUE(log))
    out[] <- log(out[] + 1)

  return(out)
}


#' @param rasLog A RasterLayer of predicted mass attack, on a log scale
#' @param propPineRas A RasterLayer of proportion pine cover.
#' @param thresholdAttackTreesMinDetectable A scalar. This will have come from
#'   an \code{optim} call to estimate this using AUC and ROC. THIS IS NOT ON A LOG
#'   SCALE. It will be logged internally.
#' @param thresholdPineProportion A scalar. The proportion of pine
#' @param years A scalar indicating how many years are included in the \code{rasLog}
#' @return
#' A \code{RasterLayer} with ever
cleanUpPredictionRas <- function(rasLog, propPineRas,
                                 thresholdAttackTreesMinDetectable = 1.4,
                                 thresholdPineProportion = 0.3) {
  rasLog[rasLog[] < log(thresholdAttackTreesMinDetectable * 12)] <- NA # 12 years ... hard coded, yikes! # TODO
  # ratioLog <- exp(mean(log(rasLog[]), na.rm = TRUE)/mean(log(mam[]), na.rm = TRUE))
  # ratio <- mean(rasLog[], na.rm = TRUE)/mean(mam[], na.rm = TRUE)
  # rasLog[] <- rasLog[] # / ratio
  if (!is.null( thresholdPineProportion)) {
    thresholdPineProportion <- 0.4
    makeNA <- propPineRas[] <= 0
    propPineRas[makeNA] <- NA
    makeNA <- propPineRas[] <= thresholdPineProportion | makeNA
    rasLog[makeNA] <- NA
    rasLog[] <- rasLog[]*propPineRas[]
  }

  rasLog
}


#' @import data.table
#' @export
centroidChange <- function(stk, propPineRas) {
  mats <- xyFromCell(stk, cell = seq(ncell(stk)))
  masDT <- as.data.table(stk[])
  bb <- data.table(propPine = propPineRas[], mats, masDT, pixel = seq(ncell(stk)))
  cc22 <- data.table::melt(bb, measure.vars = patterns("^X"), value.name = "nmAttackedTrees")
  cc22 <- cc22[!is.na(nmAttackedTrees)]
  cc22[, `:=`(sumByX = sum(nmAttackedTrees, na.rm = TRUE)),
     by = c("x", "variable")]
  cc22[, `:=`(sumByY = sum(nmAttackedTrees, na.rm = TRUE)),
     by = c("y", "variable")]
  #xs <- cc22[, .SD[1], by = c("x", "variable")]
  #ys <- cc22[, .SD[1], by = c("y", "variable")]
  xs <- cc22[, .SD[1], by = c("y", "variable")]
  ys <- cc22[, .SD[1], by = c("x", "variable")]

  setkeyv(xs, c("variable", "x", "y"))#; setkey(uniX, "x")
  setkeyv(ys, c("variable", "x", "y"))#; setkey(uniX, "x")
  keepsY <- c("variable", "propPine", "x", "sumByX")
  keepsX <- c("variable", "propPine", "y", "sumByY")
  setkeyv(xs, c("variable", "y"))#; setkey(uniX, "x")
  setkeyv(ys, c("variable", "x"))#; setkey(uniX, "x")
  ys <- unique(ys[, ..keepsY], by = c("x", "variable"))
  xs <- unique(xs[, ..keepsX], by = c("y", "variable"))
  ys[, cumsumSumByX := cumsum(sumByX), by = c("variable")]
  xs[, cumsumSumByY := cumsum(sumByY), by = c("variable")]
  yAtMax <- xs[, list(yAtMax = y[max(which(cumsumSumByY < (max(cumsumSumByY)/2) ))]), by = "variable"]
  xAtMax <- ys[, list(xAtMax = x[max(which(cumsumSumByX < (max(cumsumSumByX)/2) ))]), by = "variable"]
  centroids <- yAtMax[xAtMax]
  centroids
}

