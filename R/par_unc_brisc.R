#' Semi-variogram parameter uncertainty - BRISC
#'
#' @param BRISC_Out
#' @param n_boot
#' @param h
#' @param n_omp
#' @param init
#' @param verbose
#' @param nugget_status
#'
#' @return
#' @export
#'
#' @examples
par_uncertainty_brisc = function(BRISC_Out, B = 1000, h = 1, n_omp = 1,
                                 init = "Initial", verbose = TRUE,
                                 nugget_status = 1){
  sds = tryCatch(
    expr = {
      boot = BRISC::BRISC_bootstrap(BRISC_Out = BRISC_Out, n_boot = B, h = h, n_omp = n_omp,
                             init = init, verbose = verbose,
                             nugget_status = nugget_status)
      boot.pars = boot$boot.Theta
      boot.pars = cbind(boot.pars[,2], boot.pars[,1], 1/boot.pars[,3])

      sds = apply(boot.pars, 2, sd)
    },
    error = function(e){
      message('** ERR at ', Sys.time(), " **")
      sds = rep(NA, 3)
    }
  )
  names(sds) = c("n.sd.mle","s.sd.mle","p.sd.mle")
  return(list(sds = sds))
}
