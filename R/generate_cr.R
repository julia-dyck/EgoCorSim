#' Generate a complete realization
#'
#' @param true.nugget True nugget parameter
#' @param true.sigma_sq True partial sill parameter
#' @param true.phi True range parameter
#' @param nmax Maximum number of pairwise correlations?
#'
#' @return
#' @export
#'
#' @examples
generate_cr = function(true.nugget = 60, true.sigma_sq = 40, true.phi = 200, nmax = 100){
  D_range = 10000

  # creating random grid
  x = stats::runif(250000, min = 0, max = D_range)
  y = stats::runif(250000, min = 0, max = D_range)
  xy = as.data.frame(cbind(x,y))
  colnames(xy) <- c('x','y')
  sp::coordinates(xy) = ~x+y

  model = gstat::vgm(nugget = true.nugget, psill = true.sigma_sq, range = true.phi, model = "Exp")

  true.model = gstat::gstat(formula = z~1,
                         locations =~x+y,
                         dummy = T,         # passt wohl so (= F liefert auch einen error)
                         beta = 0,          # mean des stationary random field = 0 (constant)
                         model = model,
                         nmax = nmax)

  cr = stats::predict(true.model, newdata = xy, nsim = 1)
  return(cr = as.data.frame(cr))
}
