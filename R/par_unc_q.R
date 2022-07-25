par_unc_q = function(sample.geo, max.dist, B=1000, qu=seq(from=0.75,to=1,by=0.05)){

  # INPUT VARIABLES
  # sample.geo = a data set of class geo.data
  # max.dist = maximal distance for the estimation of the empirical semivariogram
  # B = number of bootstrap-estimates, that go into the parameter uncertainty estimation
  # qu = quantile up to which the bootstrap-resamples go into the parameter estimation
  ### note: the total nr. of bootstrap estimates calculated within the process is B/qu
  ### filtering, which resamples are included in the sd calculation is based on the quantile
  coords = sample.geo[[1]]
  z = sample.geo[[2]]

  # (1) nscore transformation
  nscore.obj = nscore(z)
  y = nscore.obj$nscore
  y.with.coords = cbind(coords,y)
  y.geo = as.geodata(y.with.coords)
  # (2) prep sv-model
  emp.sv = variog(y.geo, estimator.type="classical", max.dist = max.dist, uvec = 10, messages=F)
  ini.partial.sill <- var(y.geo[[2]])
  ini.shape <- emp.sv$max.dist/3
  ini.values <- c(ini.partial.sill, ini.shape)
  sv.mod <- variofit(emp.sv, ini.cov.pars = ini.values, cov.model = "exponential")
  mod.pars = c(sv.mod$nugget, sv.mod$cov.pars[1],sv.mod$cov.pars[2])
  # (3)
  Dist_mat = dist1(coords) # NxN distance matrix
  Cov_mat = cov.spatial(Dist_mat, cov.model=c("exponential","pure.nugget"),
                        cov.pars = rbind(c(mod.pars[2],mod.pars[3]),c(mod.pars[1],0)))
  # NxN Covariance matrix, contains all point-pairs' estimated Covariances
  # based on sv.mod
  # (4) Cholesky decomposition -> fertige Fkt. existieren
  L = t(chol(Cov_mat))
  # (5) transform y in an iid sample
  y.iid = solve(L)%*%y

  # (6),(7),(8) and (10)
  qu.min = qu[1]
  par.est = t(replicate(ceiling(B/qu.min),one.resample.analysis(platzhalter = NULL, y.iid=y.iid,
                                                                L=L, nscore.obj = nscore.obj,
                                                                coords = coords, max.dist = max.dist)))
  # 1. col = nugget estimates
  # 2. col = partial.sill estimates
  # 3. col = phi estimates

  # threshold: qu.min quantile
  sds=c(sd((par.est[,1][order(par.est[,1])])[1:B]),
        sd((par.est[,2][order(par.est[,2])])[1:B]),
        sd((par.est[,3][order(par.est[,3])])[1:B]))
  cis=c(quantile((par.est[,1][order(par.est[,1])])[1:B], probs=c(0.025,0.975)),
        quantile((par.est[,1][order(par.est[,2])])[1:B], probs=c(0.025,0.975)),
        quantile((par.est[,1][order(par.est[,3])])[1:B], probs=c(0.025,0.975)))
  # thresholds: qu[2]-qu[max] (in increasing order)
  for(q in 2:length(qu)){
    ind = sample(1:ceiling(B/qu.min), size = ceiling(B/qu[q]), replace=FALSE)
    par.est.subsample = par.est[ind,]
    sds = c(sds, c(sd((par.est.subsample[,1][order(par.est.subsample[,1])])[1:B]),
                   sd((par.est.subsample[,2][order(par.est.subsample[,2])])[1:B]),
                   sd((par.est.subsample[,3][order(par.est.subsample[,3])])[1:B])))
    cis = c(cis, c(quantile((par.est.subsample[,1][order(par.est.subsample[,1])])[1:B], probs=c(0.025,0.975)),
                   quantile((par.est.subsample[,2][order(par.est.subsample[,2])])[1:B], probs=c(0.025,0.975)),
                   quantile((par.est.subsample[,3][order(par.est.subsample[,3])])[1:B], probs=c(0.025,0.975))))
  }
  # # 95%-percentile bootstrap Confidence Intervals (pbCI) based on B resamples
  # ind = sample(1:ceiling(B/qu.min), size = B, replace=FALSE)
  # par.est.subsample = par.est[ind,]
  # pbCI = c(quantile(par.est.subsample[,1], probs = c(0.025, 0.975)),
  #          quantile(par.est.subsample[,2], probs = c(0.025, 0.975)),
  #          quantile(par.est.subsample[,3], probs = c(0.025, 0.975)))

  names(sds) = paste0(rep(c("n.sd","s.sd","p.sd"),length(qu)), as.vector(sapply(qu*100,rep, times=3)))
  names(cis) = paste0(c(rep("n",2),rep("s",2),rep("p",2)), ".ci",c("l","u"),as.vector(sapply(qu*100,rep, times=6)))

  #return(list(mod.pars=mod.pars, sds = sds, resample.ests = par.est, pbCI=pbCI))
  return(list(sds = sds, cis = cis))
}
