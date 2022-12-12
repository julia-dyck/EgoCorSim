#' Title
#'
#' @param input Input vector: Consisting of N (Number of samples), nr_divisions (for spatial_sampling function) and max.dist
#' @param cr Complete realization
#' @param nbins Number of bins for empirical semivariogram estimation
#' @param B Number of bootstrap samples
#' @param threshold.factor threshold vector
#' @param qu quantile vector
#' @param n_sim number of resamples to draw
#'
#' @return Saves an RData file to the working directory
#' @export
#'
#' @examples

one_scenario = function(input, cr, nbins, B = 1000,
                         threshold.factor = c(1.1, 1.2, 1.5, 2.0, 2.5, 3.0),
                         qu = seq(from = 0.75, to = 1, by = 0.05),
                         n_sim){
  N = input[1]
  nr_divisions = input[2]
  max.dist = input[3]

  fit.method = 7
  thr_length = length(threshold.factor)
  qu_length = length(qu)

  result = matrix(data = NA, nrow = n_sim, ncol = (3 + 6*thr_length) + (3 + 6*qu_length) + 6)

  for (i in 1:n_sim){
    sample = spatial_sampling(cr, N, nr_divisions)
    # check:
    sv.mod = EgoCor::vario.mod(data = sample, max.dist = max.dist, nbins = nbins,
                                 fit.method = fit.method, shinyresults = F)
    unc_check = par_uncertainty_check(vario.mod.output = sv.mod, mod.nr = 1, B = B,
                                        threshold.factor = threshold.factor, fit.method = fit.method)
    # quantile:
    unc_q = par_uncertainty_q(sample.geo = sample, max.dist = max.dist, nbins = nbins, B = B,
                                qu = qu, fit.method = fit.method)
    # BRISC:
    unc_brisc = par_uncertainty_brisc(sample = sample, B = B)
    # Anzahl verworfenen dazwischen
    result[i,] = c(unc_check, unc_check[1:3], unc_q, unc_brisc)
    set.seed(Sys.time())
  }


  # creating column names
  names_est_check = paste0(c("nugget", "partial_sill", "shape"), "_check")
  names_est_q = paste0(c("nugget", "partial_sill", "shape"), "_q")
  names_sd_check = paste0(c("n.sd", "ps.sd", "s.sd", "nr_reest_gstat", "nr_reest_thr", "nr_overlap"), "_check_", as.vector(sapply(threshold.factor, rep, times = 6)))
  names_sd_q = paste0(c("n.sd.q","ps.sd.q","s.sd.q", "nr_reest_gstat", "nr_reest_neg", "nr_overlap"), "_q_", as.vector(sapply(qu, rep, times = 6)))
  names_brisc = paste0(c("nugget", "partial_sill", "shape", "n.sd", "ps.sd", "s.sd"), "_brisc")

  result = as.data.frame(result)
  colnames(result) = c(names_est_check, names_sd_check, names_est_q, names_sd_q, names_brisc)

  # saving result as RData object to working directory
  save(result,
       file = paste0(
         "boot_result_",
         N, "_",
         nr_divisions, "_",
         round(max.dist,0), "_",
         ".RData"
       ))
}
