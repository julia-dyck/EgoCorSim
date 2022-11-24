one_resample_analysis_q = function(platzhalter, y.iid, L, nscore.obj, coords, max.dist, nbins, fit.method){
  # (6) resampling from y.iid
  resmpl = sample(y.iid, size = length(y.iid), replace = T)

  # (7) recorrelate the resmpl
  resmpl = L%*%resmpl

  # (8) backtransformation of the sample
  resmpl = EgoCor:::backtr(resmpl, nscore = nscore.obj, tails="none", draw=F)

  # (9) repeat steps (6)-(8) <- repeating (6),(7),(8) and (10) is done by
  #                             repeating this function application

  # (10) semivariogram model estimation, wls

  w = 0
  neg = 0

  if (fit.method == 8){
    wls.est = tryCatch(EgoCor:::sv.sep2_nlm(resmpl, coords = coords, max.dist = max.dist, nbins = nbins),
                       warning = function(w) w)
    if(methods::is(wls.est, "warning")){
      w = 1
      wls.est = EgoCor:::sv.sep2(resmpl, coords = coords, max.dist = max.dist, nbins = nbins, fit.method = fit.method)
    }
  }
  else{
    wls.est = tryCatch(EgoCor:::sv.sep2(resmpl, coords = coords, max.dist = max.dist, nbins = nbins, fit.method = fit.method),
                       warning = function(w) w)
    if(methods::is(wls.est, "warning")){
      w = 1
      wls.est = EgoCor:::sv.sep2(resmpl, coords = coords, max.dist = max.dist, nbins = nbins, fit.method = fit.method)
    }
  }

  if (sum(wls.est < 0) != 0){
    neg = 1
  }
  return(c(wls.est, w, neg))
}
