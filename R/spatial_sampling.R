#' Spatial Sampling function
#'
#' @param cr
#' @param N
#' @param nr_divisions
#'
#' @return
#' @export
#'
#' @examples
spatial_sampling = function(cr, N, nr_divisions){
  # cr = complete realizaiton, which is the set to sample from
  # D_range used in complete.realization.fct to create cr
  d = 10000
  # N = size of the sample
  # sample_subs = number of area divisions
  #               -> density parameter (low nr. of sq = low density, high nr. of sq = high density)
  if(nr_divisions == 1){
    sample.index = sample(1:dim(cr)[1], size = N , replace = F)
    sp.sample = cr[sample.index,]
  }
  if(nr_divisions == 4){
    sub.ind1 = which((0 < cr[,1]) & (cr[,1] < d/2) & (d/2 < cr[,2]) & (cr[,2] < d))
    sub.ind2 = which((d/2 < cr[,1]) & (cr[,1] < d) & (0 < cr[,2]) & (cr[,2] < d/2))
    sample.index1 = sample(sub.ind1, size = N/2 , replace = F)
    sample.index2 = sample(sub.ind2, size = N/2 , replace = F)
    sp.sample = cr[c(sample.index1,sample.index2),]
  }
  if(nr_divisions == 9){
    sub.ind1 = which(((2*d)/3 < cr[,1]) & (cr[,1] < d) & ((2*d)/3 < cr[,2]) & (cr[,2] < d))
    sub.ind2 = which((0 < cr[,1]) & (cr[,1] < d/3) & (d/3 < cr[,2]) & (cr[,2] < (2*d)/3))
    sub.ind3 = which((d/3 < cr[,1]) & (cr[,1] < (2*d)/3) & (d/3 < cr[,2]) & (cr[,2] < (2*d)/3))
    sub.ind4 = which(((2*d)/3 < cr[,1]) & (cr[,1] < d) & (0 < cr[,2]) & (cr[,2] < d/3))
    sample.index1 = sample(sub.ind1, size = N/4, replace = F)
    sample.index2 = sample(sub.ind2, size = N/4, replace = F)
    sample.index3 = sample(sub.ind3, size = N/4, replace = F)
    sample.index4 = sample(sub.ind4, size = N/4, replace = F)
    sp.sample = cr[c(sample.index1, sample.index2, sample.index3, sample.index4),]
  }

  return(sp.sample)
}
