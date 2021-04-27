#' @export
growthFunction <- function(x, s, dataset) {
  # switch(P(sim)$dataset,
  switch(dataset,
         "Berryman1979_fit" = {
           # function(x, s, dataset) {
           # TODO: check this works
           m <- lm(log10Rt ~ poly(log10Xtm1, 3, raw = TRUE), data = mod$growthData)
           s * unname(predict(m, newdata = data.frame(log10Xtm1 = x)))
           # }
         },
         "Berryman1979_forced" = {
           #function(x, s) {
           # TODO: check this works
           poly3.params <- c(1.1, -0.2, -0.9, -0.24)
           s * (poly3.params[4] * x^3 + poly3.params[3] * x^2 + poly3.params[2] * x + poly3.params[1])
           #}
         },
         "Boone2011" = {
           #function(x, s) {
           ## x is number of attacked trees (ATKTREES)
           ## s is scaling parameter (0,1), based on climate (CLIMATE)

           ## mortality from emigration/dispersal
           # r: relative stocking value (0,1)
           # d: slope parameter [1,Inf)
           # s: scaling parameter (0,1)
           m_e <- function(r, d, s) {
             s * exp(1 - d * r)
           }

           # use 2004 data as baseline for unweakened hosts (i.e., a good year for trees)
           m <- lm(amc::logit(PropKilled) ~ log(Attacked), data = subset(mod$growthData, Year == "2004"))
           a <- 0.85            ## a: slope parameter, how quickly the curve drops off
           d <- 3               ## d: slope parameter [1,Inf)
           r <- 0.2             ## r: relative stocking value (0,1) ## TODO: link this to stand depletion
           ## s: scaling parameter (0,1) -- provided by climate suitability map
           yint2 <- 0.9         ## from MacQuarrie 2011 (Fig 3d); TODO: extract from raw data
           yint <- yint2 + 0.3  ## somewhat arbitrary; chosen so that the resulting curve passes 1 when flexed

           # resulting function
           log(amc::hill(m$coefficients[[1]], m$coefficients[[2]], exp(a * x))) +
             (yint - m_e(r, d, s) - 0.03 * exp(a * x))
           #}
         }
  )
}

#' @export
xt <- function(xtminus1, cs, dataset, massAttacksMap) {
  map.res <- xres(massAttacksMap) ## TODO: fix this, not scoped in this fn
  per.ha <- 10^growthFunction(log10(xtminus1), cs, dataset) * xtminus1 ## TODO: something is off about this
  return(map.res * per.ha)
}
