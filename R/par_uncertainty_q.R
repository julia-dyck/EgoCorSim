#' Semi-variogram parameter uncertainty - Quantile method
#'
#' @param sample.geo A spatial sample
#' @param max.dist Maximum distance included in the estimation
#' @param nbins Number of bins for empirical semivariogram
#' @param B Number of bootstrap samples
#' @param qu Sequence of quantiles
#' @param fit.method Gstat fit method that is used
#'
#' @return
#' @export
#'
#' @examples
par_uncertainty_q = function(sample.geo, max.dist, nbins = 10, B=1000, qu = seq(from = 0.75, to = 1, by = 0.05), fit.method = 7){

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

  if (fit.method == 8){
  theta.star0 = log(c(.1, ini.partial.sill, ini.shape))
  sv.mod = stats::nlm(f = EgoCor:::loss, p = theta.star0, h = emp.sv$dist, gamma_hat = emp.sv$gamma,
               n_h = emp.sv$np)
  mod.pars = exp(sv.mod$estimate)
  }
  if (fit.method != 8){
  v = gstat::vgm(psill = ini.partial.sill, model = "Exp", range = ini.shape, nugget = 0)
  sv.mod = gstat::fit.variogram(emp.sv, model = v,  # fitting the model with starting model
                                fit.sills = TRUE,
                                fit.ranges = TRUE,
                                fit.method = fit.method,
                                debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)

  mod.pars = c(sv.mod$psill[1], sv.mod$psill[2], sv.mod$range[2])
  }

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
  Cov_mat = Cov_mat + diag(rep(1e-15, nrow(sample.geo)))

  L = t(chol(Cov_mat))
  # (5) transform y in an iid sample
  y.iid = solve(L)%*%y

  # (6),(7),(8) and (10)

  qu = sort(qu, decreasing = T)
  B_tilde = ceiling(B/qu)
  B_tilde.fd = diff(B_tilde)
  B_tilde = c(B_tilde[1], B_tilde.fd)
  nr_reest_gstat = numeric(length(qu))
  nr_reest_neg = numeric(length(qu))
  nr_overlap = numeric(length(qu))
  sd_nugget = numeric(length(qu))
  sd_partial.sill = numeric(length(qu))
  sd_range = numeric(length(qu))
  par.est = matrix(data = NA, nrow = 0, ncol = 5)

  for (i in 1:length(B_tilde)){
    par.est.step = t(sapply(rep(0, B_tilde[i]), FUN = one_resample_analysis_q, y.iid=y.iid,
                       L=L, nscore.obj = nscore.obj, coords = coords, max.dist = max.dist,
                       nbins = nbins, fit.method = fit.method))
    nr_notnull = sum(apply(as.matrix(par.est.step[,4:5]), 1, sum) == 0)

    while(nr_notnull < B_tilde[i]){
      par.reest = t(sapply(rep(0, B_tilde[i] - nr_notnull), FUN = one_resample_analysis_q, y.iid=y.iid,
                           L=L, nscore.obj = nscore.obj, coords = coords, max.dist = max.dist,
                           nbins = nbins, fit.method = fit.method))
      par.est.step = rbind(par.est.step, par.reest)
      nr_notnull = sum(apply(as.matrix(par.est.step[,4:5]), 1, sum) == 0)
    }

    par.est = rbind(par.est, par.est.step)
    nr_reest_gstat[i] = sum(par.est[,4] == 1)
    nr_reest_neg[i] = sum(par.est[,5] == 1)
    nr_overlap[i] = sum(par.est[,4] == 1 & par.est[,5] == 1)

    # get sds
    par.est.fine = par.est[which(par.est[,4] == 0 & par.est[,5] == 0),]

    sd_nugget[i] = stats::sd(par.est.fine[,1][order(par.est.fine[,1])][1:B])
    sd_partial.sill[i] = stats::sd(par.est.fine[,2][order(par.est.fine[,2])][1:B])
    sd_range[i] = stats::sd(par.est.fine[,3][order(par.est.fine[,3])][1:B])
  }

  sds = numeric(0)

  for (i in 1:length(qu)){
    result = c(sd_nugget[i], sd_partial.sill[i], sd_range[i], nr_reest_gstat[i], nr_reest_neg[i], nr_overlap[i])
    names(result) = paste0(c("n.sd.q","ps.sd.q","s.sd.q", "nr_reest_gstat", "nr_reest_neg", "nr_overlap"), "_", qu[i])
    sds = c(sds, result)
  }

  est = mod.pars
  names(est) = c("nugget", "partial.sill", "shape")

  #return(list(mod.pars=mod.pars, sds = sds, resample.ests = par.est, pbCI=pbCI))
  return(c(est, sds))
}

