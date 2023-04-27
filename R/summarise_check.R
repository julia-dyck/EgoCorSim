#' Compute summary statistics for the simulation of the check based method
#'
#' @param wd working directory were ALL the simulation files lie
#' @param N Number of observations (500, 1000, 2000)
#' @param density Sampling density (1, 4, 9)
#' @param max.dist Maximum distance (457, 624, 832)
#' @param thr Threshold factor (here only scalar possible)
#' @param param Selects the desired parameter (1 = nugget, 2 = partial_sill, 3 = shape)
#' @param n_runs Number of seperate files for each scenario
#' @param filter Vector for kicking out models
#'
#' @return Returns a list containing the n's and the result
#' @export
#'
#' @examples
summarise_check = function(wd = "/Users/jan-ole/R/Boot", N = NULL,
                     density = NULL, max.dist = NULL, thr = NULL,
                     param,
                     n_runs = 10, filter = c(1000, 1000, 1000)){
setwd(wd)

# Loading the datasets ----------------------------------------------------

# if N is specified
if(!is.null(N) & is.null(density) & is.null(max.dist) & is.null(thr)){

  load(file = paste0("boot_result_", N, "_1_457_", 1,".RData"))
  d1 = result
  load(file = paste0("boot_result_", N, "_1_624_", 1, ".RData"))
  d2 = result
  load(file = paste0("boot_result_", N, "_1_832_", 1, ".RData"))
  d3 = result
  load(file = paste0("boot_result_", N, "_4_457_", 1, ".RData"))
  d4 = result
  load(file = paste0("boot_result_", N, "_4_624_", 1, ".RData"))
  d5 = result
  load(file = paste0("boot_result_", N, "_4_832_", 1, ".RData"))
  d6 = result
  load(file = paste0("boot_result_", N, "_9_457_", 1, ".RData"))
  d7 = result
  load(file = paste0("boot_result_", N, "_9_624_", 1, ".RData"))
  d8 = result
  load(file = paste0("boot_result_", N, "_9_832_", 1, ".RData"))
  d9 = result
  data = rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)

  for (i in 2:n_runs){
    load(file = paste0("boot_result_", N, "_1_457_", i,".RData"))
    d1 = result
    load(file = paste0("boot_result_", N, "_1_624_", i, ".RData"))
    d2 = result
    load(file = paste0("boot_result_", N, "_1_832_", i, ".RData"))
    d3 = result
    load(file = paste0("boot_result_", N, "_4_457_", i, ".RData"))
    d4 = result
    load(file = paste0("boot_result_", N, "_4_624_", i, ".RData"))
    d5 = result
    load(file = paste0("boot_result_", N, "_4_832_", i, ".RData"))
    d6 = result
    load(file = paste0("boot_result_", N, "_9_457_", i, ".RData"))
    d7 = result
    load(file = paste0("boot_result_", N, "_9_624_", i, ".RData"))
    d8 = result
    load(file = paste0("boot_result_", N, "_9_832_", i, ".RData"))
    d9 = result
    d = rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)

    data = rbind(data, d)
  }
}


# if density is specified
if(!is.null(density) & is.null(N) & is.null(max.dist) & is.null(thr)){

  load(file = paste0("boot_result_500_", density, "_457_", 1, ".RData"))
  d1 = result
  load(file = paste0("boot_result_500_", density, "_624_", 1, ".RData"))
  d2 = result
  load(file = paste0("boot_result_500_", density, "_832_", 1, ".RData"))
  d3 = result
  load(file = paste0("boot_result_1000_", density, "_457_", 1, ".RData"))
  d4 = result
  load(file = paste0("boot_result_1000_", density, "_624_", 1, ".RData"))
  d5 = result
  load(file = paste0("boot_result_1000_", density, "_832_", 1, ".RData"))
  d6 = result
  load(file = paste0("boot_result_2000_", density, "_457_", 1, ".RData"))
  d7 = result
  load(file = paste0("boot_result_2000_", density, "_624_", 1, ".RData"))
  d8 = result
  load(file = paste0("boot_result_2000_", density, "_832_", 1, ".RData"))
  d9 = result
  data = rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)

  for (i in 2:n_runs){
    load(file = paste0("boot_result_500_", density, "_457_", i, ".RData"))
    d1 = result
    load(file = paste0("boot_result_500_", density, "_624_", i, ".RData"))
    d2 = result
    load(file = paste0("boot_result_500_", density, "_832_", i, ".RData"))
    d3 = result
    load(file = paste0("boot_result_1000_", density, "_457_", i, ".RData"))
    d4 = result
    load(file = paste0("boot_result_1000_", density, "_624_", i, ".RData"))
    d5 = result
    load(file = paste0("boot_result_1000_", density, "_832_", i, ".RData"))
    d6 = result
    load(file = paste0("boot_result_2000_", density, "_457_", i, ".RData"))
    d7 = result
    load(file = paste0("boot_result_2000_", density, "_624_", i, ".RData"))
    d8 = result
    load(file = paste0("boot_result_2000_", density, "_832_", i, ".RData"))
    d9 = result
    d = rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)

    data = rbind(data, d)
  }
}


# if max.dist is specified
if(!is.null(max.dist) & is.null(N) & is.null(density) & is.null(thr)){

  load(file = paste0("boot_result_500_1_", max.dist, "_", 1, ".RData"))
  d1 = result
  load(file = paste0("boot_result_500_4_", max.dist, "_", 1, ".RData"))
  d2 = result
  load(file = paste0("boot_result_500_9_", max.dist, "_", 1, ".RData"))
  d3 = result
  load(file = paste0("boot_result_1000_1_", max.dist, "_", 1, ".RData"))
  d4 = result
  load(file = paste0("boot_result_1000_4_", max.dist, "_", 1, ".RData"))
  d5 = result
  load(file = paste0("boot_result_1000_9_", max.dist, "_", 1, ".RData"))
  d6 = result
  load(file = paste0("boot_result_2000_1_", max.dist, "_", 1, ".RData"))
  d7 = result
  load(file = paste0("boot_result_2000_4_", max.dist, "_", 1, ".RData"))
  d8 = result
  load(file = paste0("boot_result_2000_9_", max.dist, "_", 1, ".RData"))
  d9 = result
  data = rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)

  for (i in 2:n_runs){
    load(file = paste0("boot_result_500_1_", max.dist, "_", i, ".RData"))
    d1 = result
    load(file = paste0("boot_result_500_4_", max.dist, "_", i, ".RData"))
    d2 = result
    load(file = paste0("boot_result_500_9_", max.dist, "_", i, ".RData"))
    d3 = result
    load(file = paste0("boot_result_1000_1_", max.dist, "_", i, ".RData"))
    d4 = result
    load(file = paste0("boot_result_1000_4_", max.dist, "_", i, ".RData"))
    d5 = result
    load(file = paste0("boot_result_1000_9_", max.dist, "_", i, ".RData"))
    d6 = result
    load(file = paste0("boot_result_2000_1_", max.dist, "_", i, ".RData"))
    d7 = result
    load(file = paste0("boot_result_2000_4_", max.dist, "_", i, ".RData"))
    d8 = result
    load(file = paste0("boot_result_2000_9_", max.dist, "_", i, ".RData"))
    d9 = result

    d = rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)

    data = rbind(data, d)
  }
}


# if threshold is specified
if(!is.null(thr) & is.null(N) & is.null(max.dist) & is.null(density)){
  load(file = paste0("boot_result_500_1_457_", 1, ".RData"))
  d1 = result
  load(file = paste0("boot_result_500_1_624_", 1, ".RData"))
  d2 = result
  load(file = paste0("boot_result_500_1_832_", 1, ".RData"))
  d3 = result
  load(file = paste0("boot_result_500_4_457_", 1, ".RData"))
  d4 = result
  load(file = paste0("boot_result_500_4_624_", 1, ".RData"))
  d5 = result
  load(file = paste0("boot_result_500_4_832_", 1, ".RData"))
  d6 = result
  load(file = paste0("boot_result_500_9_457_", 1, ".RData"))
  d7 = result
  load(file = paste0("boot_result_500_9_624_", 1, ".RData"))
  d8 = result
  load(file = paste0("boot_result_500_9_832_", 1, ".RData"))
  d9 = result

  load(file = paste0("boot_result_1000_1_457_", 1, ".RData"))
  d10 = result
  load(file = paste0("boot_result_1000_1_624_", 1, ".RData"))
  d11 = result
  load(file = paste0("boot_result_1000_1_832_", 1, ".RData"))
  d12 = result
  load(file = paste0("boot_result_1000_4_457_", 1, ".RData"))
  d13 = result
  load(file = paste0("boot_result_1000_4_624_", 1, ".RData"))
  d14 = result
  load(file = paste0("boot_result_1000_4_832_", 1, ".RData"))
  d15 = result
  load(file = paste0("boot_result_1000_9_457_", 1, ".RData"))
  d16 = result
  load(file = paste0("boot_result_1000_9_624_", 1, ".RData"))
  d17 = result
  load(file = paste0("boot_result_1000_9_832_", 1, ".RData"))
  d18 = result

  load(file = paste0("boot_result_2000_1_457_", 1, ".RData"))
  d19 = result
  load(file = paste0("boot_result_2000_1_624_", 1, ".RData"))
  d20 = result
  load(file = paste0("boot_result_2000_1_832_", 1, ".RData"))
  d21 = result
  load(file = paste0("boot_result_2000_4_457_", 1, ".RData"))
  d22 = result
  load(file = paste0("boot_result_2000_4_624_", 1, ".RData"))
  d23 = result
  load(file = paste0("boot_result_2000_4_832_", 1, ".RData"))
  d24 = result
  load(file = paste0("boot_result_2000_9_457_", 1, ".RData"))
  d25 = result
  load(file = paste0("boot_result_2000_9_624_", 1, ".RData"))
  d26 = result
  load(file = paste0("boot_result_2000_9_832_", 1, ".RData"))
  d27 = result

  data = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25,d26,d27)

  for (i in 2:n_runs){
    load(file = paste0("boot_result_500_1_457_", i, ".RData"))
    d1 = result
    load(file = paste0("boot_result_500_1_624_", i, ".RData"))
    d2 = result
    load(file = paste0("boot_result_500_1_832_", i, ".RData"))
    d3 = result
    load(file = paste0("boot_result_500_4_457_", i, ".RData"))
    d4 = result
    load(file = paste0("boot_result_500_4_624_", i, ".RData"))
    d5 = result
    load(file = paste0("boot_result_500_4_832_", i, ".RData"))
    d6 = result
    load(file = paste0("boot_result_500_9_457_", i, ".RData"))
    d7 = result
    load(file = paste0("boot_result_500_9_624_", i, ".RData"))
    d8 = result
    load(file = paste0("boot_result_500_9_832_", i, ".RData"))
    d9 = result

    load(file = paste0("boot_result_1000_1_457_", i, ".RData"))
    d10 = result
    load(file = paste0("boot_result_1000_1_624_", i, ".RData"))
    d11 = result
    load(file = paste0("boot_result_1000_1_832_", i, ".RData"))
    d12 = result
    load(file = paste0("boot_result_1000_4_457_", i, ".RData"))
    d13 = result
    load(file = paste0("boot_result_1000_4_624_", i, ".RData"))
    d14 = result
    load(file = paste0("boot_result_1000_4_832_", i, ".RData"))
    d15 = result
    load(file = paste0("boot_result_1000_9_457_", i, ".RData"))
    d16 = result
    load(file = paste0("boot_result_1000_9_624_", i, ".RData"))
    d17 = result
    load(file = paste0("boot_result_1000_9_832_", i, ".RData"))
    d18 = result

    load(file = paste0("boot_result_2000_1_457_", i, ".RData"))
    d19 = result
    load(file = paste0("boot_result_2000_1_624_", i, ".RData"))
    d20 = result
    load(file = paste0("boot_result_2000_1_832_", i, ".RData"))
    d21 = result
    load(file = paste0("boot_result_2000_4_457_", i, ".RData"))
    d22 = result
    load(file = paste0("boot_result_2000_4_624_", i, ".RData"))
    d23 = result
    load(file = paste0("boot_result_2000_4_832_", i, ".RData"))
    d24 = result
    load(file = paste0("boot_result_2000_9_457_", i, ".RData"))
    d25 = result
    load(file = paste0("boot_result_2000_9_624_", i, ".RData"))
    d26 = result
    load(file = paste0("boot_result_2000_9_832_", i, ".RData"))
    d27 = result

    d = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25,d26,d27)

    data = rbind(data, d)
  }
}


# Loading data completed --------------------------------------------------


n_sim = nrow(data)

# filtering the data: Only models where all estimates are < the selected value (default 1000)
data_f = data[which(data$nugget_check < filter[1] &
                      data$partial_sill_check < filter[2] &
                      data$shape_check < filter[3]), ]

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
    mc = mean(data_f$partial_sill_check)
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
    mc = mean(data_f$shape_check)
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
    mc = mean(data_f$partial_sill_check)
    emp_se = mean(
      data_f[,which(colnames(data_f) == paste0("ps.sd_check_", thr))]
    )
    se_emp_se = sd(
      data_f[,which(colnames(data_f) == paste0("ps.sd_check_", thr))]
    )
  }

  if(param == 3){ # shape
    mc = mean(data_f$shape_check)
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

