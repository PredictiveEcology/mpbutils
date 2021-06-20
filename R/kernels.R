#' 2Dt kernel
#'
#' This is a 2-dimensional t-distribution that can be used for dispersal of natural
#' entities. It has many characteristics (see Clark et al 1999)
#'
#' @references
#' Clark, J. S., M. Silman, R. Kern, E. Macklin, and J. HilleRisLambers. 1999.
#' Seed dispersal near and far: patterns across temperate and tropical forests.
#' Ecology 80:1475–1494.
#'
#' @param dist A vector of distances
#' @param mu   The first parameter of the 2Dt kernel. This represents about 0.9 of the
#'             mean dispersal distance
#'
#' @param p    The second parameter of the 2Dt kernel. This changes the shape.
#' @export
#' @return
#' A vector of probabilities
kernel_twoDT <- function(dist, mu, p) {
  (2*mu^p)/(dist^(2*p+1)*exp(lgamma(p)))*exp(-mu/(dist)^2)
}

#' @rdname kernel_twoDT
kernel_twoDT_mean <- function(mu, m) 2*mu/(m*exp(lgamma(m/2)))
