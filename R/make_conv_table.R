#' convergence rate table
#'
#' @param wd working directory were ALL the simulation files lie
#' @param N Number of observations (500, 1000, 2000)
#' @param dens Sampling density (1, 4, 9)
#' @param max.dist Maximum distance (457, 624, 832)
#' @param n_runs Number of seperate files for each scenario
#' @param filter Vector with upper non-convergence threshold for kicking out models
#'
#' @return Returns a list containing the n's and the result
#' @export

make_convtable = function(n_runs = 1:10, summary = T){
  pc = expand.grid(max.dist = c(457, 624, 832),
                   dens = c(1, 4, 9),
                   N = c(500, 1000, 2000)
                   )
  pc = pc[,3:1]

  res = list()
  conv_t = matrix(rep(0, 6*27), ncol = 6, nrow = 27)
  for(i in 1:27){
    res[[i]] = load_scenario(wd = "D:/Sciebo/EgoCorSim_mit_Ole",
                             N = pc[i,1],
                             dens = pc[i,2],
                             max.dist = pc[i,3],
                             n_runs = 1:10,
                             filter = rep(1000,3))
    conv_t[i,] = c(N = pc[i,1], density = pc[i,2], max.dist = pc[i,3],
                   n_sim = res[[i]]$n_sim,
                   n_sim_tilde = res[[i]]$n_sim_tilde,
                   conv.rate = res[[i]]$n_sim_tilde/res[[i]]$n_sim)
  }
  colnames(conv_t) = c("N", "density", "max. distance",
                       "n_sim", "n_sim_tilde", "conv. rate")
  conv_t = data.frame(conv_t)

  if(summary == F){
    return(conv_t)
  }

  if(summary == T){
    conv_t[,1] = as.factor(conv_t[,1])
    conv_t[,2] = as.factor(conv_t[,2])
    conv_t[,3] = as.factor(conv_t[,3])
    conv_t_summary_N = by(data = conv_t[,4:6], INDICES = list(conv_t$N), FUN = colMeans)
    conv_t_summary_N = do.call(rbind, conv_t_summary_N)
    conv_t_summary_d = by(data = conv_t[,4:6], INDICES = list(conv_t$density), FUN = colMeans)
    conv_t_summary_d = do.call(rbind, conv_t_summary_d)
    conv_t_summary_md = by(data = conv_t[,4:6], INDICES = list(conv_t$max..distance), FUN = colMeans)
    conv_t_summary_md = do.call(rbind, conv_t_summary_md)
    conv_t_summary = data.frame(rbind(conv_t_summary_N, conv_t_summary_d, conv_t_summary_md))
    conv_t_summary$n_sim = conv_t_summary$n_sim*9
    conv_t_summary$n_sim_tilde = conv_t_summary$n_sim_tilde*9
    return(conv_t_summary)
  }
}


t = gen.conv.table(summary = F)
t= gen.conv.table(summary = T)
t
