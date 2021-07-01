#' Create a set of binary stacks from quantitative stacks
#'
#' This is for converting forecasted abundance stacks in a list of simList objects
#' into a list of binary stacks (presence/absence)
#'
#' @param sims a list of simLists
#' @param thresholdAttackTreesMinDetectable A scalar that delineates presence from
#'   an absense. Default is 1.4, which was derived from initial efforts to find
#'   a single value that works in all years.
#' @param thresholdPineProportion A scalar. Values on the propPineRas that are below
#'   this threshold will be masked out, i.e., set to NA.
#' @param stackName Character string. The name of the stack inside the simLists to use
#' @param propPineRasName Character string. The name of the RasterLayer inside the simLists to use
#'   for proportion Pine.
#' @rdname binaryStacks
#' @export
binaryStacks <- function(sims, thresholdAttackTreesMinDetectable = 1.4,
                         thresholdPineProportion = 0.3,
                         stackName = "predictedStack", propPineRasName = "propPineRas") {
  lapply(sims, function(sim) {
    pred <- sim[[stackName]]
    propPineRas <- sim[[propPineRasName]]
    binaryStack(pred, propPineRas = propPineRas, thresholdAttackTreesMinDetectable)
  })
}

#' Create a binary stack from a quantitative stack
#'
#' This is for converting forecasted abundance stack in a simList object
#' into a binary stack (presence/absence)
#'
#' @param stk A stack of abundance
#' @param propPineRas A RasterLayer that has values between 0 and 1, representing
#'   the proportion of pine cover in the pixel.
#' @rdname binaryStacks
#' @export
binaryStack <- function(stk, propPineRas, thresholdAttackTreesMinDetectable = 1.4,
                        thresholdPineProportion = 0.3) {
  stk <- maskWPine(stk, propPineRas, thresholdPineProportion)
  stkList <- raster::unstack(stk)
  stkList <- Map(stak = stkList, yr = seq_along(names(stk)), function(stak, yr) {
    toZero <- stak < (thresholdAttackTreesMinDetectable ^ yr)
    stak[toZero] <- 0
    stak[!toZero] <- 1
    stak
  })
  # stk[stk < thresholdAttackTreesMinDetectable] <- 0
  # stk[stk >= thresholdAttackTreesMinDetectable] <- 1
  raster::stack(stkList)
}


#' Mask a layer based on the values of another layer
#'
#' The mask will occur with values of NA or values of 0.
#'
#' @param ras A RasterLayer of abundance
#' @param propPineRas A RasterLayer that has values between 0 and 1, representing
#'   the proportion of pine cover in the pixel.
#' @rdname binaryStacks
#' @export
maskWPine <- function(ras, propPineRas, thresholdPineProportion) {
  if (!is.null( thresholdPineProportion)) {
    makeNA <- propPineRas[] <= 0
    propPineRas[makeNA] <- NA
    makeNA <- propPineRas[] <= thresholdPineProportion | makeNA
    ras[makeNA] <- NA
    ras[] <- ras[]*propPineRas[]
  }

  return(raster::stack(ras))
}

probAttack <- function(binStks) {
  yrs <- seq_len(nlayers(binStks[[1]]))
  names(yrs) <- paste0("X", yrs)
  yrProb <- lapply(yrs, function(yr) {
    yrStk <- raster::stack(lapply(binStks, function(stk) {
      stk[[yr]]
    } ))
    sum(yrStk, na.rm = TRUE)/nlayers(yrStk)
  })
  raster::stack(yrProb)
}
