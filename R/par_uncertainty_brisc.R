#' Semi-variogram parameter uncertainty - BRISC
#'
#' @param BRISC_Out An Object created by BRISC_estimation
#' @param B Number of bootstrap samples
#' @param h h
#' @param n_omp omp
#' @param init init
#' @param verbose verbose
#' @param nugget_status nugget_status
#'
#' @return
#' @export
#'
#' @examples
par_uncertainty_brisc = function(sample, B = 1000, h = 1, n_omp = 1,
                                 init = "Initial", verbose = TRUE,
                                 nugget_status = 1){
  coords = as.matrix(cbind(x = sample[,1], y = sample[,2]))

  BRISC_Out = BRISC::BRISC_estimation(coords = coords, y = sample[,3]) # was ist mit max.dist und nbins?
  est = BRISC_Out$Theta
  names(est) = c("nugget.brisc", "partial.sill.brisc", "shape.brisc")

  sds = tryCatch(
    expr = {
      boot = BRISC::BRISC_bootstrap(BRISC_Out = BRISC_Out, n_boot = B, h = h, n_omp = n_omp,
                             init = init, verbose = verbose,
                             nugget_status = nugget_status)
      boot.pars = boot$boot.Theta
      boot.pars = cbind(boot.pars[,2], boot.pars[,1], 1/boot.pars[,3])

      sds = apply(boot.pars, 2, stats::sd)
    },
    error = function(e){
      message('** ERR at ', Sys.time(), " **")
      sds = rep(NA, 3)
    }
  )
  names(sds) = c("n.sd.brisc","ps.sd.brisc","s.sd.brisc")
  return(c(est, sds))
}
