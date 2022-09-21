#' Semi-variogram parameter uncertainty - Quantile method
#'
#' @param sample.geo A spatial sample
#' @param max.dist Maximum distance included in the estimation
#' @param B Number of bootstrap samples
#' @param qu Sequence of quantiles
#'
#' @return
#' @export
#'
#' @examples
par_uncertainty_q = function(sample.geo, max.dist, nbins = 10, B=1000, qu = seq(from = 0.75, to = 1, by = 0.05)){

  # INPUT VARIABLES
  # sample.geo = a data set of class geo.data
  # max.dist = maximal distance for the estimation of the empirical semivariogram
  # B = number of bootstrap-estimates, that go into the parameter uncertainty estimation
  # qu = quantile up to which the bootstrap-resamples go into the parameter estimation
  ### note: the total nr. of bootstrap estimates calculated within the process is B/qu
  ### filtering, which resamples are included in the sd calculation is based on the quantile

  sample.geo = stats::na.omit(as.data.frame(sample.geo[,1:3]))
  colnames(sample.geo)[1:2] = c("x", "y")
  sp::coordinates(sample.geo) = ~x+y
  coords = sp::coordinates(sample.geo)
  z = sample.geo[[1]]

  # (1) nscore transformation
  nscore.obj = EgoCor:::nscore(z)
  y = nscore.obj$nscore
  y.geo = as.data.frame(cbind(coords,y))
  sp::coordinates(y.geo) = ~x+y

  # (2) prep sv-model
  emp.sv = gstat::variogram(object = y.geo[[1]] ~ 1, data = y.geo, cutoff = max.dist, width = max.dist / nbins)
  ini.partial.sill <- stats::var(y.geo[[1]])
  ini.shape <- max(emp.sv$dist)/3

  v = gstat::vgm(psill = ini.partial.sill, model = "Exp", range = ini.shape, nugget = 0)
  sv.mod = gstat::fit.variogram(emp.sv, model = v,  # fitting the model with starting model
                                fit.sills = TRUE,
                                fit.ranges = TRUE,
                                debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)

  mod.pars = c(sv.mod$psill[1], sv.mod$psill[2], sv.mod$range[2])
  # (3)
  Dist_mat = SpatialTools::dist1(coords) # NxN distance matrix

  # function for predicting the covariance based on distance
  expmod = function(distvec, psill, phi){
    return(psill*exp(-distvec/phi))
  }

  Cov_mat = apply(X = Dist_mat, MARGIN = 1, FUN = expmod, psill = mod.pars[2], phi = mod.pars[3])

  # NxN Covariance matrix, contains all point-pairs' estimated Covariances
  # based on sv.mod
  # (4) Cholesky decomposition -> fertige Fkt. existieren

  # Adding diag(epsilon) on the diagonal to force it to be positve definite (numerical reasons)
  Cov_mat = Cov_mat + diag(rep(1e-8, nrow(sample.geo)))

  L = t(chol(Cov_mat))
  # (5) transform y in an iid sample
  y.iid = solve(L)%*%y

  # (6),(7),(8) and (10)

  B_tilde = ceiling(B/qu[1])

  par.est = t(sapply(rep(0, B_tilde), FUN = one_resample_analysis, y.iid=y.iid,
                     L=L, nscore.obj = nscore.obj, coords = coords, max.dist = max.dist,
                     nbins = nbins))

  par.est = stats::na.omit(par.est)
  nr_reestimates = length(stats::na.omit(par.est[,1]))

  while(nr_reestimates < B_tilde){
    next.est = one_resample_analysis(platzhalter = NULL, y.iid=y.iid,
                                     L=L, nscore.obj = nscore.obj, coords = coords, max.dist = max.dist,
                                     nbins = nbins)
    if(!is.na(next.est[1])){
      par.est = rbind(par.est, next.est)
      nr_reestimates= nr_reestimates + 1
    }
  }

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
    ind = sample(1:ceiling(B/qu[1]), size = ceiling(B/qu[q]), replace=FALSE)
    par.est.subsample = par.est[ind,]
    sds = c(sds, c(sd((par.est.subsample[,1][order(par.est.subsample[,1])])[1:B]),
                   sd((par.est.subsample[,2][order(par.est.subsample[,2])])[1:B]),
                   sd((par.est.subsample[,3][order(par.est.subsample[,3])])[1:B])))
    cis = c(cis, c(quantile((par.est.subsample[,1][order(par.est.subsample[,1])])[1:B], probs=c(0.025,0.975), na.rm = TRUE),
                   quantile((par.est.subsample[,2][order(par.est.subsample[,2])])[1:B], probs=c(0.025,0.975), na.rm = TRUE),
                   quantile((par.est.subsample[,3][order(par.est.subsample[,3])])[1:B], probs=c(0.025,0.975), na.rm = TRUE)))
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
