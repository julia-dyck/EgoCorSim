one_scenario_check = function(input, cr, nbins, B = 1000,
                              threshold.factor = c(1.1, 1.2, 1.5, 2.0, 2.5, 3.0), n_sim){
  N = input[1]
  nr_divisions = input[2]
  max.dist = input[3]
# fit.method = input[4] raus aus scenarios --> nur Schleifen für check und

  result = matrix(data = NA, nrow = n_sim, ncol = 3 + 3*length(threshold.factor))

  for (i in 1:n_sim){
    sample = spatial_sampling(cr, N, nr_divisions)

    # Schleife über die fit.methods (mehrere punktschätzer (jeweils pro fit.method))
    sv.mod = EgoCor::vario.mod(data = sample, max.dist = max.dist, nbins = nbins,
                       fit.method = fit.method, shinyresults = F)
    unc = par_uncertainty_check(vario.mod.output = sv.mod, mod.nr = 1, B = B,
                                threshold.factor = threshold.factor, fit.method = fit.method)
    result[i,] = unc
    # noch Punktschätzer rein
  }

  colnames(result) = paste0(rep(c("se(nugget).", "se(partial.sill).", "(se(shape)."), length(threshold.factor)), as.vector(sapply(threshold.factor, rep, times = 3)))
  result = as.data.frame(result)
  save(result,
       file = paste0(
         "bootstap_result_check_",
         N, "_",
         nr_divisions, "_",
         max.dist, "_",
         fit.method,
         ".RData"
       ))
}
