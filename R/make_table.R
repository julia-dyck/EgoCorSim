#' Creating result table for simulation study fast
#'
#' @param wd working directory were ALL the simulation files lie
#' @param param Selects the desired parameter (1 = nugget, 2 = partial_sill, 3 = shape)
#' @param method one of c("check", "quantile", "brisc")
#' @param n_runs Number of seperate files for each scenario (this needs to fit to the largest integer at the end of the file names)
#'
#' @return A list containing the tables
#' @export
#'
#' @examples
make_table = function(wd, param, method = "check", n_runs){
  N = c(500, 1000, 2000)
  dens = c(1, 4, 9)
  max.dist = c(457, 624, 832)
  threshold = c(1.1, 1.2, 1.5, 2, 2.5, 3)
  qu = c(1, 0.95, 0.9, 0.85, 0.8, 0.75)
  parameter = c("nugget", "partial sill", "shape")

  if(method == "check"){
    N_table = matrix(data = NA, 3, 8)
    for (i in 1:length(N)){
      result = summarise_check(wd = wd, N = N[i], param = param, n_runs = n_runs)
      N_table[i,] = c(result$result, result$n)
    }
    colnames(N_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(N_table) = as.character(N)

    density_table = matrix(data = NA, 3, 8)
    for (i in 1:length(dens)){
      result = summarise_check(wd = wd, density = dens[i], param = param, n_runs = n_runs)
      density_table[i,] = c(result$result, result$n)
    }
    colnames(density_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(density_table) = as.character(density)

    max.dist_table = matrix(data = NA, 3, 8)
    for (i in 1:length(max.dist)){
      result = summarise_check(wd = wd, max.dist = max.dist[i], param = param, n_runs = n_runs)
      max.dist_table[i,] = c(result$result, result$n)
    }
    colnames(max.dist_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(max.dist_table) = c("1.1", "1.5", "2.0")

    threshold_table = matrix(data = NA, 6, 8)
    for (i in 1:length(threshold)){
      result = summarise_check(wd = wd, thr = threshold[i], param = param, n_runs = n_runs)
      threshold_table[i,] = c(result$result, result$n)
    }
    colnames(threshold_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(threshold_table) = as.character(threshold)

    print(paste("Performance table: Check filtering for", parameter[param], "parameter"))
    return(list(sample_size = N_table, density = density_table, maximal_distance = max.dist_table, tuning_parameter_tau = threshold_table))
  }

  if(method == "quantile"){
    N_table = matrix(data = NA, 3, 8)
    for (i in 1:length(N)){
      result = summarise_q(wd = wd, N = N[i], param = param, n_runs = n_runs)
      N_table[i,] = c(result$result, result$n)
    }
    colnames(N_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(N_table) = as.character(N)

    density_table = matrix(data = NA, 3, 8)
    for (i in 1:length(dens)){
      result = summarise_q(wd = wd, density = dens[i], param = param, n_runs = n_runs)
      density_table[i,] = c(result$result, result$n)
    }
    colnames(density_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(density_table) = as.character(density)

    max.dist_table = matrix(data = NA, 3, 8)
    for (i in 1:length(max.dist)){
      result = summarise_q(wd = wd, max.dist = max.dist[i], param = param, n_runs = n_runs)
      max.dist_table[i,] = c(result$result, result$n)
    }
    colnames(max.dist_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(max.dist_table) = c("1.1", "1.5", "2.0")

    qu_table = matrix(data = NA, 6, 8)
    for (i in 1:length(qu)){
      result = summarise_q(wd = wd, qu = qu[i], param = param, n_runs = n_runs)
      qu_table[i,] = c(result$result, result$n)
    }
    colnames(qu_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(qu_table) = as.character(qu)

    print(paste("Performance table: Quantile filtering for", parameter[param], "parameter"))
    return(list(sample_size = N_table, density = density_table, maximal_distance = max.dist_table, tuning_parameter_alpha = qu_table))
  }

  if(method == "brisc"){
    N_table = matrix(data = NA, 3, 8)
    for (i in 1:length(N)){
      result = summarise_BRISC(wd = wd, N = N[i], param = param, n_runs = n_runs)
      N_table[i,] = c(result$result, result$n)
    }
    colnames(N_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(N_table) = as.character(N)

    density_table = matrix(data = NA, 3, 8)
    for (i in 1:length(dens)){
      result = summarise_BRISC(wd = wd, density = dens[i], param = param, n_runs = n_runs)
      density_table[i,] = c(result$result, result$n)
    }
    colnames(density_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(density_table) = as.character(density)

    max.dist_table = matrix(data = NA, 3, 8)
    for (i in 1:length(max.dist)){
      result = summarise_BRISC(wd = wd, max.dist = max.dist[i], param = param, n_runs = n_runs)
      max.dist_table[i,] = c(result$result, result$n)
    }
    colnames(max.dist_table) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE", "n_sim", "n_sim_tilde")
    rownames(max.dist_table) = c("1.1", "1.5", "2.0")

    print(paste("Performance table: BRISC method for", parameter[param], "parameter"))
    return(list(sample_size = N_table, density = density_table, maximal_distance = max.dist_table))
  }
}
