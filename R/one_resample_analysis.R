
one_resample_analysis = function(platzhalter, y.iid, L, nscore.obj, coords, max.dist){
  # (6) resampling from y.iid
  resmpl = sample(y.iid, size = length(y.iid), replace = T)

  # (7) recorrelate the resmpl
  resmpl = L%*%resmpl

  # (8) backtransformation of the sample
  resmpl = backtr(resmpl, nscore = nscore.obj, tails="none", draw=F)

  # (9) repeat steps (6)-(8) <- repeating (6),(7),(8) and (10) is done by
  #                             repeating this function application

  # (10) semivariogram model estimation, wls
  wls.est = sv.sep(resmpl, coords = coords, max.dist = max.dist)

  if (sum(wls.est < 0) == 0){
    return(wls.est)
  }
  else {
    return(c(NA,NA,NA))
  }
}
