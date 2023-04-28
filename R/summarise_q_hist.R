#' Compute summary statistics for the simulation of the quantile based method
#'
#' @param wd working directory were ALL the simulation files lie
#' @param N Number of observations (500, 1000, 2000)
#' @param density Sampling density (1, 4, 9)
#' @param max.dist Maximum distance (457, 624, 832)
#' @param qu Quantile (here only scalar possible)
#' @param param Selects the desired parameter (1 = nugget, 2 = partial_sill, 3 = shape)
#' @param n_runs Number of seperate files for each scenario
#' @param filter Vector for kicking out models
#'
#' @return Returns a list containing the n's and the result
#' @export
#'
#' @examples
summarise_q_hist = function(wd = "/Users/jan-ole/R/Boot", qu = NULL, path = "/Users/jan-ole/R/Boot",
                       n_runs = 10, filter = c(1000, 1000, 1000)){
  setwd(wd)

  # Loading the datasets ----------------------------------------------------

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

  # Loading data completed --------------------------------------------------

  n_sim = nrow(data)
  # filtering the data: Only models where all estimates are < the selected value (default 1000)
  data_f = data[which(data$nugget_check < filter[1] &
                        data$partial_sill_check < filter[2] &
                        data$shape_check < filter[3]), ]
  n_sim_tilde = nrow(data_f)

  res = data.frame(
    nugget_check = data_f$nugget_q,
    partial_sill_check = data_f$partial_sill_q,
    shape_check = data_f$shape_q,
    n.sd = data_f[,which(colnames(data_f) == paste0("n.sd.q_q_", qu))],
    ps.sd = data_f[,which(colnames(data_f) == paste0("ps.sd.q_q_", qu))],
    s.sd = data_f[,which(colnames(data_f) == paste0("s.sd.q_q_", qu))]
  )

  file = paste0(path, "/emp_dist_parests_and_seests_q_", qu, ".pdf")

  pdf(file = file,width = 9, height = 6)

  par(mfrow = c(2,3), mar = c(4, 4, 3, 2), font.main = 1)
  br = 30

  mains = latex2exp::TeX(c(paste("Empirical distribution of",
                                 c("$c_0$", "$\\sigma^2_0$", "$\\phi$",
                                   "$\\eta_{c_0}$", "$\\eta_{\\sigma^2_0}$", "$\\eta_{\\phi}$"))))
  xlabels = latex2exp::TeX(c("$\\hat{c}_0$", "$\\hat{\\sigma}^2_0$", "$\\hat{\\phi}$",
                             "$\\widehat{\\eta_{c_0}}$", "$\\widehat{\\eta_{\\sigma^2_0}}$", "$\\widehat{\\eta_{\\phi}}$"))

  for(colu in 1:6){
    r = res[,colu]
    hist(r,
         xlim = c(0,max(r)),
         main = mains[colu],
         breaks = br, prob = T,
         xlab = xlabels[colu])

    abline(v = mean(r), col = "red", lty = 2, lwd = 2)
    abline(v = quantile(r, c(0.5)), col = "blue", lty = 3, lwd = 2)
    lines(seq(0, max(r), by = 0.1), dnorm(seq(0, max(r), by = 0.1), mean(r),sd(r)), lty = 1, lwd = 2)
    if(colu == 3){
      legend("topright", legend = c("mean", "median", "normal", "density"),
             col = c("red","blue","black", "white"), lty = c(2,3,1,1), lwd = 2, cex = 0.6)

    }
  }
  dev.off()

  # plotting again

  par(mfrow = c(2,3), mar = c(4, 4, 3, 2), font.main = 1)
  br = 30

  for(colu in 1:6){
    r = res[,colu]
    hist(r,
         xlim = c(0,max(r)),
         main = mains[colu],
         breaks = br, prob = T,
         xlab = xlabels[colu])

    abline(v = mean(r), col = "red", lty = 2, lwd = 2)
    abline(v = quantile(r, c(0.5)), col = "blue", lty = 3, lwd = 2)
    lines(seq(0, max(r), by = 0.1), dnorm(seq(0, max(r), by = 0.1), mean(r),sd(r)), lty = 1, lwd = 2)
    if(colu == 3){
      legend("topright", legend = c("mean", "median", "normal", "density"),
             col = c("red","blue","black", "white"), lty = c(2,3,1,1), lwd = 2, cex = 0.6)

    }
  }
}

