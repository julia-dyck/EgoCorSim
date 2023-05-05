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

make_convtable = function(n_runs = 1:10, summary = T, kick_equal0 = F){
  pc = expand.grid(max.dist = c(457, 624, 832),
                   dens = c(1, 4, 9),
                   N = c(500, 1000, 2000)
                   )
  pc = pc[,3:1]

  res = list()
  conv_t = matrix(rep(0, 8*27), ncol = 8, nrow = 27)
  for(i in 1:27){
    res[[i]] = load_scenario(wd = "D:/Sciebo/EgoCorSim_mit_Ole",
                             N = pc[i,1],
                             dens = pc[i,2],
                             max.dist = pc[i,3],
                             n_runs = 1:10,
                             filter = c(rep(0,3), rep(1000,3)),
                             kick_equal0 = kick_equal0)
    conv_t[i,] = c(N = pc[i,1], density = pc[i,2], max.dist = pc[i,3],
                   n_sim = res[[i]]$n_sim,
                   removed_lower = res[[i]]$removed_lower_thr,
                   removed_upper = res[[i]]$removed_upper_thr,
                   n_sim_tilde = res[[i]]$n_sim_tilde,
                   conv.rate = res[[i]]$n_sim_tilde/res[[i]]$n_sim)
  }
  if(kick_equal0 == T){
    colnames(conv_t) = c("N", "density", "max.distance",
                         "n_sim", "n_smallerequal0", "n_larger1000",
                         "n_sim_tilde", "conv.rate")
  }
  else{
    colnames(conv_t) = c("N", "density", "max.distance",
                         "n_sim", "n_smaller0", "n_larger1000",
                         "n_sim_tilde", "conv.rate")
  }


  conv_t = data.frame(conv_t)

  if(summary == F){
    return(conv_t)
  }

  if(summary == T){
    conv_ts = matrix(rep(0, 6*9), ncol = 6, nrow = 9)
    conv_ts[1,] = c(500,
                  sum(conv_t[which(conv_t$N == 500),"n_sim"]),
                  sum(conv_t[which(conv_t$N == 500),"n_smallerequal0"]),
                  sum(conv_t[which(conv_t$N == 500),"n_larger1000"]),
                  sum(conv_t[which(conv_t$N == 500),"n_sim_tilde"]),
                  0
                  )
    conv_ts[2,] = c(1000,
                    sum(conv_t[which(conv_t$N == 1000),"n_sim"]),
                    sum(conv_t[which(conv_t$N == 1000),"n_smallerequal0"]),
                    sum(conv_t[which(conv_t$N == 1000),"n_larger1000"]),
                    sum(conv_t[which(conv_t$N == 1000),"n_sim_tilde"]),
                    0
    )
    conv_ts[3,] = c(2000,
                    sum(conv_t[which(conv_t$N == 2000),"n_sim"]),
                    sum(conv_t[which(conv_t$N == 2000),"n_smallerequal0"]),
                    sum(conv_t[which(conv_t$N == 2000),"n_larger1000"]),
                    sum(conv_t[which(conv_t$N == 2000),"n_sim_tilde"]),
                    0
    )
    conv_ts[4,] = c(1,
                    sum(conv_t[which(conv_t$density == 1),"n_sim"]),
                    sum(conv_t[which(conv_t$density == 1),"n_smallerequal0"]),
                    sum(conv_t[which(conv_t$density == 1),"n_larger1000"]),
                    sum(conv_t[which(conv_t$density == 1),"n_sim_tilde"]),
                    0
    )
    conv_ts[5,] = c(4,
                    sum(conv_t[which(conv_t$density == 4),"n_sim"]),
                    sum(conv_t[which(conv_t$density == 4),"n_smallerequal0"]),
                    sum(conv_t[which(conv_t$density == 4),"n_larger1000"]),
                    sum(conv_t[which(conv_t$density == 4),"n_sim_tilde"]),
                    0
    )
    conv_ts[6,] = c(9,
                     sum(conv_t[which(conv_t$density == 9),"n_sim"]),
                     sum(conv_t[which(conv_t$density == 9),"n_smallerequal0"]),
                     sum(conv_t[which(conv_t$density == 9),"n_larger1000"]),
                     sum(conv_t[which(conv_t$density == 9),"n_sim_tilde"]),
                     0
    )
    conv_ts[7,] = c(457,
                    sum(conv_t[which(conv_t$max.distance == 457),"n_sim"]),
                    sum(conv_t[which(conv_t$max.distance == 457),"n_smallerequal0"]),
                    sum(conv_t[which(conv_t$max.distance == 457),"n_larger1000"]),
                    sum(conv_t[which(conv_t$max.distance == 457),"n_sim_tilde"]),
                    0
    )
    conv_ts[8,] = c(624,
                    sum(conv_t[which(conv_t$max.distance == 624),"n_sim"]),
                    sum(conv_t[which(conv_t$max.distance == 624),"n_smallerequal0"]),
                    sum(conv_t[which(conv_t$max.distance == 624),"n_larger1000"]),
                    sum(conv_t[which(conv_t$max.distance == 624),"n_sim_tilde"]),
                    0
    )
    conv_ts[9,] = c(832,
                    sum(conv_t[which(conv_t$max.distance == 832),"n_sim"]),
                    sum(conv_t[which(conv_t$max.distance == 832),"n_smallerequal0"]),
                    sum(conv_t[which(conv_t$max.distance == 832),"n_larger1000"]),
                    sum(conv_t[which(conv_t$max.distance == 832),"n_sim_tilde"]),
                    0
    )
    conv_ts = data.frame(conv_ts)
    if(kick_equal0 == T){
      colnames(conv_ts) = c("parameter", "n_sim", "n_smallerequal0", "n_larger1000",
                            "n_sim_tilde", "conv.rate")
    }
    else{
      colnames(conv_ts) = c("parameter", "n_sim", "n_smaller0", "n_larger1000",
                            "n_sim_tilde", "conv.rate")
    }
    conv_ts$conv.rate = conv_ts$n_sim_tilde/conv_ts$n_sim
    return(conv_ts)

    return(conv_t_summary)
  }
}

# round(make_convtable(summary = T, kick_equal0 = T),2)
# round(make_convtable(summary = T, kick_equal0 = F),2)
# round(make_convtable(summary = F, kick_equal0 = T),2)
# round(make_convtable(summary = F, kick_equal0 = F),2)

