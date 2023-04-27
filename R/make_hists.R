




make_hists = function(n_runs = 1:10, summary = F, allscenarios = F){
  pc = expand.grid(max.dist = c(457, 624, 832),
                   dens = c(1, 4, 9),
                   N = c(500, 1000, 2000)
  )
  pc = pc[,3:1]

  res = list()
  for(i in 1:27){
    res[[i]] = load_scenario(wd = "D:/Sciebo/EgoCorSim_mit_Ole",
                             N = pc[i,1],
                             dens = pc[i,2],
                             max.dist = pc[i,3],
                             n_runs = 1:10,
                             filter = rep(1000,3))
    res[[i]] = res[[i]]$data
  }
  if(allscenarios == T){
    res_all = do.call(rbind, res)
  }
  # HIER WEITER
  # Ziel: histogramme zum parameter und
  #       zu den se sch?tzungen f?r alle drei parameter


  mypath = "D:/J/Sciebo/bootstrap_uncertainty_modifications/article/graphics"
  file_name = paste0("/emp_dist_parests_and_seests_sc", sc, ".png")

  png(file= paste0(mypath, file_name),
      width = 1500, height = 900, units = "px", pointsize = 40)
  par(mfrow = c(2,3),
      mar = c(4, 4, 3, 2),
      font.main = 1)
  #generate graphics # NEW DEF OF COLU needed above!
  br = 30
  mains = TeX(c(paste("Empirical distribution of",
                      c("$c_0$", "$\\sigma^2_0$", "$\\sigma^2_0$",
                        "$\\eta_{c_0}$", "$\\eta_{\\sigma^2_0}$", "$\\eta_{\\phi}$"))))
  xlabels = TeX(c("$\\hat{c}_0$", "$\\hat{\\sigma}^2_0$", "$\\hat{\\phi}$",
                  "$\\widehat{\\eta_{c_0}}$", "$\\widehat{\\eta_{\\sigma^2_0}}$", "$\\widehat{\\eta_{\\phi}}$"))

  for(colu in 1:6){
    r = res[,colu]
    hist(r,
         xlim = c(0,max(r)),
         main = mains[colu],
         breaks = br, prob = T,
         xlab = xlabels[colu])

    abline(v = mean(r), col = "red", lty = 2, lwd = 2)
    abline(v = quantile(r, c(0.5)), col = "blue", lty = 3, lwd = 2)
    lines(seq(0, max(r), by = 0.1), dnorm(seq(0, max(r), by = 0.1), mean(r),sd(r)), lty = 1, lwd = 2)
    if(colu == 3){
      legend("topright", legend = c("mean", "median", "normal", "density"),
             col = c("red","blue","black", "white"), lty = c(2,3,1,1), lwd = 2, cex = 0.6)

    }
  }
  dev.off()


}

t = make_hists(allscenarios = T)
