#' Load single scenario (without summarizing)
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
#'
#'
load_scenario = function(wd = "D:/Sciebo/EgoCorSim_mit_Ole",
                         N = 500,
                         dens = 1,
                         max.dist = 457,
                         n_runs = 1:10,
                         filter = rep(1000,3)){
  # load the first batch
  filename = paste0(wd, "/", "boot_result_",
                    N, "_", dens, "_", max.dist, "_",
                    n_runs[1], ".RData")
  load(file = filename)
  df = result
  # load and rbind all remaining batches
  for(i in 2:length(n_runs)){
    filename = paste0(wd, "/", "boot_result_",
                      N, "_", dens, "_", max.dist, "_",
                      n_runs[i], ".RData")
    load(file = filename)
    dfi = result
    df = rbind(df, dfi)
  }
  n_sim = nrow(df)
  #filtering the data: Only models where all estimates are < the selected value (default 1000)
  df = df[which(df$nugget_check < filter[1] &
                df$partial_sill_check < filter[2] &
                df$shape_check < filter[3]), ]
  n_sim_tilde_interm = nrow(df)
  # filtering the data: Only models where all estimates are >= 0
  df = df[which(df$nugget_check >= 0 &
                df$partial_sill_check >= 0 &
                df$shape_check >= 0), ]

  n_sim_tilde = nrow(df)
  return(list(data = df, n_sim = n_sim,
              n_sim_tilde = n_sim_tilde,
              n_sim_tilde_intermediate = n_sim_tilde_interm))
}

dat = load_scenario()
dat[[1]]
dat[[2]]
dat[[3]]
dat[[4]]
