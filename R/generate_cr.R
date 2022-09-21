#' Title
#'
#' @return
#' @export
#'
#' @examples
generate_cr = function(){
  true.nugget = 60
  true.sigma_sq = 40
  true.phi = 200
  D_range = 10000
  true_covariogram = RMexp(var = true.sigma_sq,
                           scale = true.phi)+RMnugget(var = true.nugget) # true model
  unifx = runif(250000, min = 0, max = D_range) # gen x coords
  unify = runif(250000, min = 0, max = D_range) # gen y coords
  # -> simulate the whole population of the neighbourhood (ein Stadtteil)
  spatial.data = RFsimulate(model = true_covariogram, x = unifx, y = unify, n = 1) # gen. n=1 realizaton
  complete.realiz = cbind(spatial.data@coords,spatial.data@data)
  return(complete.realiz)
}
