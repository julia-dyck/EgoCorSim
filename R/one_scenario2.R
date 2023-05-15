one_scenario2 = function(input, cr, nbins, B = 1000,
                        threshold.factor = c(1.1, 1.2, 1.5, 2.0, 2.5, 3.0),
                        qu = seq(from = 0.75, to = 1, by = 0.05), # decreasing order!!!
                        n_sim){
  N = input[1]
  nr_divisions = input[2]
  max.dist = input[3]

  fit.methods = c(1,2,6,7,8)
  J = length(fit.methods)
  thr_length = length(threshold.factor)
  qu_length = length(qu)

  result = matrix(data = NA, nrow = n_sim,
                  ncol =
                    5*(3 + 3*thr_length) + # check
                    5*(3 + 3*qu_length) + # quantile
                    6) # brisc

  for (i in 1:n_sim){
    sample = spatial_sampling(cr, N, nr_divisions)

    # check based and quantile based mathod in loop over fit methods
    unc_check_all_fms = numeric(5*(3 + 3*length(threshold.factor)))
    unc_q_all_fms = numeric(5*(3 + 3*length(qu)))

    for (j in 1:J){
      # check:
      sv.mod = EgoCor::vario.mod(data = sample, max.dist = max.dist, nbins = nbins,
                                 fit.method = fit.methods[j], shinyresults = F)
      unc_check = par_uncertainty_check(vario.mod.output = sv.mod, mod.nr = 1, B = B,
                                    threshold.factor = threshold.factor, fit.method = fit.methods[j])
     # names(unc_check) = paste0(names(unc_check), "_fm", fit.method)
      unc_check_all_fms[((j-1)*(3 + 3*thr_length))+1:(3 + 3*thr_length)] = unc_check

      # quantile:
      unc_q = par_uncertainty_q(sample.geo = sample, max.dist = max.dist, nbins = nbins, B = B,
                                qu = qu, fit.method = fit.methods[j])
     # names(unc_q) = paste0(names(unc_q), "_fm", fit.method)

      unc_q_all_fms[((j-1)*(3 + 3*qu_length))+1:(3 + 3*qu_length)] = unc_q
    }

    # BRISC:
    unc_brisc = par_uncertainty_brisc(sample = sample, B = B)
    result[i,] = c(unc_check_all_fms, unc_q_all_fms, unc_brisc)

    set.seed(Sys.time())
  }

  names_est_check = paste0(c("nugget", "partial_sill", "shape"), "_check", "_fm", as.vector(sapply(fit.methods, rep, times = 3)))
  names_est_q = paste0(c("nugget", "partial_sill", "shape"), "_q", "_fm", as.vector(sapply(fit.methods, rep, times = 3)))
  names_sd_check = numeric(0)
  for (fit.method in fit.methods){
    names_sd_check = c(names_sd_check, paste0(c("n.sd", "ps.sd", "s.sd"), "_check_", as.vector(sapply(threshold.factor, rep, times = 3)), "_fm", fit.method))
  }
  names_sd_q = numeric(0)
  for (fit.method in  fit.methods){
    names_sd_q = c(names_sd_q, paste0(c("n.sd", "ps.sd", "s.sd"), "_q_", as.vector(sapply(qu, rep, times = 3)), "_fm", fit.method))
  }
  names_check = numeric(0)
  for (j in 1:length(fit.methods)){
    names_check = c(names_check, names_est_check[3*(j-1)+1:3], names_sd_check[3*thr_length*(j-1)+1:(3*thr_length)])
  }
  names_q = numeric(0)
  for (j in 1:length(fit.methods)){
    names_q = c(names_q, names_est_q[3*(j-1)+1:3], names_sd_q[3*qu_length*(j-1)+1:(3*qu_length)])
  }
  names_brisc = paste0(c("nugget", "partial_sill", "shape", "n.sd", "ps.sd", "s.sd"), "_brisc")
  names = c(names_check, names_q, names_brisc)

  result = as.data.frame(result)
  colnames(result) = names

  save(result,
       file = paste0(
         "bootstap_result_",
         N, "_",
         nr_divisions, "_",
         round(max.dist,0), "_",
         ".RData"
       ))
}
