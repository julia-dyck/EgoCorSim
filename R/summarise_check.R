#' Compute summary statistics for the simulation of the check based method
#'
#' @param wd working directory were ALL the simulation files lie
#' @param N Number of observations (500, 1000, 2000)
#' @param density Sampling density (1, 4, 9)
#' @param max.dist Maximum distance (457, 624, 832)
#' @param thr Threshold factor (here only scalar possible)
#' @param param Selects the desired parameter (1 = nugget, 2 = partial_sill, 3 = shape)
#' @param filter Vector for kicking out models
#'
#' @return Returns a list containing the n's and the result
#' @export
#'
#' @examples
summarise_check = function(wd = "/Users/jan-ole/R/Boot", N = NULL,
                     density = NULL, max.dist = NULL, thr = NULL,
                     param,
                     filter = c(1000, 1000, 1000),
                     kick_equal0 = F){
setwd(wd)

# Loading the datasets ----------------------------------------------------

# if N is specified
if(!is.null(N) & is.null(density) & is.null(max.dist) & is.null(thr)){

  load(file = paste0("boot_result_", N, "_1_457_",".RData"))
  d1 = result
  load(file = paste0("boot_result_", N, "_1_624_", ".RData"))
  d2 = result
  load(file = paste0("boot_result_", N, "_1_832_", ".RData"))
  d3 = result
  load(file = paste0("boot_result_", N, "_4_457_",".RData"))
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
if(!is.null(density) & is.null(N) & is.null(max.dist) & is.null(thr)){

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
if(!is.null(max.dist) & is.null(N) & is.null(density) & is.null(thr)){

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


# if threshold is specified
if(!is.null(thr) & is.null(N) & is.null(max.dist) & is.null(density)){
  load(file = paste0("boot_result_500_1_457_", ".RData"))
  d1 = result
  load(file = paste0("boot_result_500_1_624_", ".RData"))
  d2 = result
  load(file = paste0("boot_result_500_1_832_", ".RData"))
  d3 = result
  load(file = paste0("boot_result_500_4_457_", ".RData"))
  d4 = result
  load(file = paste0("boot_result_500_4_624_", ".RData"))
  d5 = result
  load(file = paste0("boot_result_500_4_832_", ".RData"))
  d6 = result
  load(file = paste0("boot_result_500_9_457_", ".RData"))
  d7 = result
  load(file = paste0("boot_result_500_9_624_", ".RData"))
  d8 = result
  load(file = paste0("boot_result_500_9_832_", ".RData"))
  d9 = result

  load(file = paste0("boot_result_1000_1_457_", ".RData"))
  d10 = result
  load(file = paste0("boot_result_1000_1_624_", ".RData"))
  d11 = result
  load(file = paste0("boot_result_1000_1_832_", ".RData"))
  d12 = result
  load(file = paste0("boot_result_1000_4_457_", ".RData"))
  d13 = result
  load(file = paste0("boot_result_1000_4_624_", ".RData"))
  d14 = result
  load(file = paste0("boot_result_1000_4_832_", ".RData"))
  d15 = result
  load(file = paste0("boot_result_1000_9_457_", ".RData"))
  d16 = result
  load(file = paste0("boot_result_1000_9_624_", ".RData"))
  d17 = result
  load(file = paste0("boot_result_1000_9_832_", ".RData"))
  d18 = result

  load(file = paste0("boot_result_2000_1_457_", ".RData"))
  d19 = result
  load(file = paste0("boot_result_2000_1_624_", ".RData"))
  d20 = result
  load(file = paste0("boot_result_2000_1_832_", ".RData"))
  d21 = result
  load(file = paste0("boot_result_2000_4_457_", ".RData"))
  d22 = result
  load(file = paste0("boot_result_2000_4_624_", ".RData"))
  d23 = result
  load(file = paste0("boot_result_2000_4_832_", ".RData"))
  d24 = result
  load(file = paste0("boot_result_2000_9_457_", ".RData"))
  d25 = result
  load(file = paste0("boot_result_2000_9_624_", ".RData"))
  d26 = result
  load(file = paste0("boot_result_2000_9_832_", ".RData"))
  d27 = result

  data = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25,d26,d27)

}


# Loading data completed --------------------------------------------------


n_sim = nrow(data)

# filtering the data: Only models where all estimates are
# >= or > the lower support boundary 0
# < the selected value (default 1000)
data_f = data[which(data$nugget_check < filter[1] &
                    data$partial_sill_check < filter[2] &
                    data$shape_check < filter[3]), ]

if(kick_equal0 == F){
  data_f = data_f[which(0 <= data_f$nugget_check &
                        0 <= data_f$partial_sill_check &
                        0 <= data_f$shape_check), ]
}
if(kick_equal0 == T){
  data_f = data_f[which(0 < data_f$nugget_check &
                        0 < data_f$partial_sill_check &
                        0 < data_f$shape_check), ]
}

n_sim_tilde = nrow(data_f)

# if no threshold is supplied we need to glue all columns together

if(is.null(thr)){
  if(param == 1){ # nugget
    mc = sd(data_f$nugget_check)
    emp_se = mean(
      c(data_f[,which(colnames(data_f) == paste0("n.sd_check_", 1.1))],
        data_f[,which(colnames(data_f) == paste0("n.sd_check_", 1.2))],
        data_f[,which(colnames(data_f) == paste0("n.sd_check_", 1.5))],
        data_f[,which(colnames(data_f) == paste0("n.sd_check_", 2))],
        data_f[,which(colnames(data_f) == paste0("n.sd_check_", 2.5))],
        data_f[,which(colnames(data_f) == paste0("n.sd_check_", 3))])
    )
    se_emp_se = sd(
      c(data_f[,which(colnames(data_f) == paste0("n.sd_check_", 1.1))],
        data_f[,which(colnames(data_f) == paste0("n.sd_check_", 1.2))],
        data_f[,which(colnames(data_f) == paste0("n.sd_check_", 1.5))],
        data_f[,which(colnames(data_f) == paste0("n.sd_check_", 2))],
        data_f[,which(colnames(data_f) == paste0("n.sd_check_", 2.5))],
        data_f[,which(colnames(data_f) == paste0("n.sd_check_", 3))])
    )
  }

  if(param == 2){ # partial_sill
    mc = sd(data_f$partial_sill_check)
    emp_se = mean(
      c(data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 1.1))],
        data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 1.2))],
        data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 1.5))],
        data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 2))],
        data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 2.5))],
        data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 3))])
      )
    se_emp_se = sd(
      c(data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 1.1))],
        data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 1.2))],
        data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 1.5))],
        data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 2))],
        data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 2.5))],
        data_f[,which(colnames(data_f) == paste0("ps.sd_check_", 3))])
      )
  }

  if(param == 3){ # shape
    mc = sd(data_f$shape_check)
    emp_se = mean(
      c(data_f[,which(colnames(data_f) == paste0("s.sd_check_", 1.1))],
        data_f[,which(colnames(data_f) == paste0("s.sd_check_", 1.2))],
        data_f[,which(colnames(data_f) == paste0("s.sd_check_", 1.5))],
        data_f[,which(colnames(data_f) == paste0("s.sd_check_", 2))],
        data_f[,which(colnames(data_f) == paste0("s.sd_check_", 2.5))],
        data_f[,which(colnames(data_f) == paste0("s.sd_check_", 3))])
    )
    se_emp_se = sd(
      c(data_f[,which(colnames(data_f) == paste0("s.sd_check_", 1.1))],
        data_f[,which(colnames(data_f) == paste0("s.sd_check_", 1.2))],
        data_f[,which(colnames(data_f) == paste0("s.sd_check_", 1.5))],
        data_f[,which(colnames(data_f) == paste0("s.sd_check_", 2))],
        data_f[,which(colnames(data_f) == paste0("s.sd_check_", 2.5))],
        data_f[,which(colnames(data_f) == paste0("s.sd_check_", 3))])
    )
  }
  mc_se = mc/sqrt(2*(n_sim_tilde-1))
  bias = emp_se - mc
  MSE = se_emp_se^2 + bias^2
}

# if a threshold is supplied we only use the corresponding columns
if(!is.null(thr)){
  if(param == 1){ # nugget
    mc = sd(data_f$nugget_check)
    emp_se = mean(
      data_f[,which(colnames(data_f) == paste0("n.sd_check_", thr))]
    )
    se_emp_se = sd(
      data_f[,which(colnames(data_f) == paste0("n.sd_check_", thr))]
    )
  }

  if(param == 2){ # partial_sill
    mc = sd(data_f$partial_sill_check)
    emp_se = mean(
      data_f[,which(colnames(data_f) == paste0("ps.sd_check_", thr))]
    )
    se_emp_se = sd(
      data_f[,which(colnames(data_f) == paste0("ps.sd_check_", thr))]
    )
  }

  if(param == 3){ # shape
    mc = sd(data_f$shape_check)
    emp_se = mean(
      data_f[,which(colnames(data_f) == paste0("s.sd_check_", thr))]
    )
    se_emp_se = sd(
      data_f[,which(colnames(data_f) == paste0("s.sd_check_", thr))]
    )
  }

  mc_se = mc/sqrt(2*(n_sim_tilde-1))
  bias = emp_se - mc
  MSE = se_emp_se^2 + bias^2
}


out = c(mc, mc_se, emp_se, se_emp_se, bias, MSE)
names(out) = c("Monte Carlo SE", "sd(Monte Carlo SE)", "empirical SE", "sd(empirical SE)", "bias", "MSE")

ns = c(n_sim, n_sim_tilde)
names(ns) = c("n_sim", "n_sim_tilde")
return(list(n = ns, result = out, data_nsim = data_f))
}

