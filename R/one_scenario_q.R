one_scenario_q = function(input, cr, max.dist, nbins, fit.method = 7,
                              B = 1000, qu = seq(from = 0.75, to = 1, by = 0.05),
                              n_sim){
  N = input[1]
  nr_divisions = input[2]
  # max.dist (1.1, 1.5, 2)*wahre prac. range
  # fit.method (1,2,6,7,8)

  result = matrix(data = NA, nrow = n_sim, ncol = 3 + 3*length(qu))
  # result matrix für alle Methoden (Spalten hintereinander (entsprechend benennen))

  for (i in 1:n_sim){
    sample = spatial_sampling(cr, N, nr_divisions)

    unc = par_uncertainty_q(sample.geo = sample, max.dist = max.dist, nbins = nbins, B = B,
                                qu = qu, fit.method = fit.method)


    # hier noch andere Methoden

    result[i,] = unc
    # noch Punktschätzer rein
  }

  colnames(result) = c("nugget", "partiall.sill", "range",
                       paste0(rep(c("n.sd","s.sd","p.sd"),length(qu)), as.vector(sapply(qu*100,rep, times=3))))
  result = as.data.frame(result)
  save(result,
       file = paste0(
         "bootstap_result_q_",
         N, "_",
         nr_divisions,
         max.dist, "_",
         fit.method,
         ".RData"
       ))
}
