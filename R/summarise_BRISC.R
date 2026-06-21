#' Compute summary statistics for the simulation of the BRISC method
#'
#' @param wd working directory were ALL the simulation files lie
#' @param N Number of observations (500, 1000, 2000)
#' @param density Sampling density (1, 4, 9)
#' @param max.dist Maximum distance (457, 624, 832)
#' @param param Selects the desired parameter (1 = nugget, 2 = partial_sill, 3 = shape)
#' @param filter Vector for kicking out models
#'
#' @return Returns a list containing the n's and the result
#' @export
#'
#' @examples
summarise_BRISC = function(wd = "/Users/jan-ole/R/Boot", N = NULL,
                           density = NULL, max.dist = NULL,
                           param, filter = c(1000, 1000, 1000),
                           kick_equal0 = F){
  setwd(wd)

  # Loading the datasets ----------------------------------------------------

  # if N is specified
  if(!is.null(N) & is.null(density) & is.null(max.dist)){

    load(file = paste0("boot_result_", N, "_1_457_",".RData"))
    d1 = result
    load(file = paste0("boot_result_", N, "_1_624_", ".RData"))
    d2 = result
    load(file = paste0("boot_result_", N, "_1_832_", ".RData"))
    d3 = result
    load(file = paste0("boot_result_", N, "_4_457_", ".RData"))
    d4 = result
    load(file = paste0("boot_result_", N, "_4_624_", ".RData"))
    d5 = result
    load(file = paste0("boot_result_", N, "_4_832_", ".RData"))
    d6 = result
    load(file = paste0("boot_result_", N, "_9_457_", ".RData"))
    d7 = result
    load(file = paste0("boot_result_", N, "_9_624_", ".RData"))
    d8 = result
    load(file = paste0("boot_result_", N, "_9_832_", ".RData"))
    d9 = result
    data = rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)
  }


  # if density is specified
  if(!is.null(density) & is.null(N) & is.null(max.dist)){

    load(file = paste0("boot_result_500_", density, "_457_", ".RData"))
    d1 = result
    load(file = paste0("boot_result_500_", density, "_624_", ".RData"))
    d2 = result
    load(file = paste0("boot_result_500_", density, "_832_", ".RData"))
    d3 = result
    load(file = paste0("boot_result_1000_", density, "_457_", ".RData"))
    d4 = result
    load(file = paste0("boot_result_1000_", density, "_624_", ".RData"))
    d5 = result
    load(file = paste0("boot_result_1000_", density, "_832_", ".RData"))
    d6 = result
    load(file = paste0("boot_result_2000_", density, "_457_", ".RData"))
    d7 = result
    load(file = paste0("boot_result_2000_", density, "_624_", ".RData"))
    d8 = result
    load(file = paste0("boot_result_2000_", density, "_832_", ".RData"))
    d9 = result
    data = rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)
  }


  # if max.dist is specified
  if(!is.null(max.dist) & is.null(N) & is.null(density)){

    load(file = paste0("boot_result_500_1_", max.dist, "_", ".RData"))
    d1 = result
    load(file = paste0("boot_result_500_4_", max.dist, "_", ".RData"))
    d2 = result
    load(file = paste0("boot_result_500_9_", max.dist, "_", ".RData"))
    d3 = result
    load(file = paste0("boot_result_1000_1_", max.dist, "_", ".RData"))
    d4 = result
    load(file = paste0("boot_result_1000_4_", max.dist, "_", ".RData"))
    d5 = result
    load(file = paste0("boot_result_1000_9_", max.dist, "_", ".RData"))
    d6 = result
    load(file = paste0("boot_result_2000_1_", max.dist, "_", ".RData"))
    d7 = result
    load(file = paste0("boot_result_2000_4_", max.dist, "_", ".RData"))
    d8 = result
    load(file = paste0("boot_result_2000_9_", max.dist, "_", ".RData"))
    d9 = result
    data = rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)
  }


  # Loading data completed --------------------------------------------------


  n_sim = nrow(data)

  # filtering the data: Only models where all estimates are < the selected value (default 1000)
  data_f = data[which(data$nugget_brisc < filter[1] &
                        data$partial_sill_brisc < filter[2] &
                        data$shape_brisc < filter[3]), ]

  if(kick_equal0 == F){
    data_f = data_f[which(0 <= data_f$nugget_brisc &
                          0 <= data_f$partial_sill_brisc &
                          0 <= data_f$shape_brisc), ]
  }
  if(kick_equal0 == T){
    data_f = data_f[which(0 < data_f$nugget_brisc &
                          0 < data_f$partial_sill_brisc &
                          0 < data_f$shape_brisc), ]
  }

  n_sim_tilde = nrow(data_f)

  if(param == 1){ # nugget
    mc = sd(data_f$nugget_brisc)
    emp_se = mean(data_f$n.sd_brisc)
    se_emp_se = sd(data_f$n.sd_brisc)
  }

  if(param == 2){ # partial_sill
    mc = sd(data_f$partial_sill_brisc)
    emp_se = mean(data_f$ps.sd_brisc)
    se_emp_se = sd(data_f$ps.sd_brisc)
  }

  if(param == 3){ # shape
    mc = sd(data_f$shape_brisc)
    emp_se = mean(data_f$s.sd_brisc)
    se_emp_se = sd(data_f$s.sd_brisc)
  }

  mc_se = mc/sqrt(2*(n_sim_tilde-1))
  bias = emp_se - mc
  MSE = se_emp_se^2 + bias^2


  out = c(mc, mc_se, emp_se, se_emp_se, bias, MSE)
  names(out) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE")

  ns = c(n_sim, n_sim_tilde)
  names(ns) = c("n_sim", "n_sim_tilde")

  return(list(n = ns, result = out, data_nsim = data_f))
}

