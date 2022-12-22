#' Creating result table for simulation study fast
#'
#' @param wd working directory were ALL the simulation files lie
#' @param param Selects the desired parameter (1 = nugget, 2 = partial_sill, 3 = shape)
#' @param method one of c("check", "quantile", "brisc")
#' @param n_runs Number of seperate files for each scenario (this needs to fit to the largest integer at the end of the file names)
#'
#' @return A LaTeX-ready version of the table is printed. The full table is also returned invisibly.
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
  p_symbol = c("c_0", "\\sigma^2_0", "\\phi")


# Check method ------------------------------------------------------------

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

    # creating LaTeX table

    table = rbind(N_table, density_table, max.dist_table, threshold_table)
    table = as.data.frame(table[,1:6])
    table = cbind(
      grouping_paramter = c(as.character(N), as.character(density), c("1.1", "1.5", "2.0"), as.character(threshold)),
      paste0(round(table[,1], 2)," (", round(table[,2], 4),")"),
      paste0(round(table[,3], 2)," (", round(table[,4], 4),")"),
      round(table[,5], 4), round(table[,6], 4)
    )
    colnames(table) = c("grouping parameter",
                        paste0("\\eta_{", p_symbol[param], "}", " (Monte Carlo SE)"),
                        paste0("\\hat{\\eta}_{", p_symbol[param], "}", " (empirical SE)"),
                        "bias", "MSE")
    rownames(table) = NULL

    ps = c("sample size", "sample density", "maximal distance factor", "tuning parameter $\\tau$")
    add = list()
    add$pos = list(0,3,6,9)
    add$command = paste0("\\hline \n \\multicolumn{", 5, "}{|l|}{\\textbf{$\\varnothing$ grouped by ", ps, "}} \\\\ \n")
    thecaption = paste("Check Filter Method - Performance measure results for the", parameter[param],"effect", "$", p_symbol[param], "$", "grouped by sample size, sample density, maximal distance factor and tuning parameter $\\tau$.")

    print(xtable::xtable(table, digits = rep(4,6), # first zero "represents" row numbers which we skip later
                         align = "r|L{2cm}R{3.5cm}R{3cm}R{2cm}R{2cm}|",  # align and put a vertical line (first "l" again represents column of row numbers)
                         caption = thecaption,
                         label = paste0("perftable_check_", parameter[param])),
          add.to.row = add,
          size = "footnotesize", #Change size; useful for bigger tables "normalsize" "footnotesize"
          include.rownames = FALSE, #Don't print rownames
          include.colnames = T,
          caption.placement = "bottom", #"top","bottom", NULL
          caption.width = "\\textwidth",
          hline.after=c(-1,nrow(t)), #We don't need hline; we use booktabs -> NULL,
          floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
          sanitize.text.function = force, # Important to treat content of first column as latex function
          table.placement="H"
    )

    return(invisible(list(sample_size = N_table, density = density_table, maximal_distance = max.dist_table, tuning_parameter_tau = threshold_table)))
  }


# Quantile Method ---------------------------------------------------------

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

    # creating LaTeX table

    table = rbind(N_table, density_table, max.dist_table, qu_table)
    table = as.data.frame(table[,1:6])
    table = cbind(
      grouping_paramter = c(as.character(N), as.character(density), c("1.1", "1.5", "2.0"), as.character(threshold)),
      paste0(round(table[,1], 2)," (", round(table[,2], 4),")"),
      paste0(round(table[,3], 2)," (", round(table[,4], 4),")"),
      round(table[,5], 4), round(table[,6], 4)
    )
    colnames(table) = c("grouping parameter",
                        paste0("\\eta_{", p_symbol[param], "}", " (Monte Carlo SE)"),
                        paste0("\\hat{\\eta}_{", p_symbol[param], "}", " (empirical SE)"),
                        "bias", "MSE")
    rownames(table) = NULL

    ps = c("sample size", "sample density", "maximal distance factor", "tuning parameter $\\alpha$")
    add = list()
    add$pos = list(0,3,6,9)
    add$command = paste0("\\hline \n \\multicolumn{", 5, "}{|l|}{\\textbf{$\\varnothing$ grouped by ", ps, "}} \\\\ \n")
    thecaption = paste("Check Filter Method - Performance measure results for the", parameter[param],"effect", "$", p_symbol[param], "$", "grouped by sample size, sample density, maximal distance factor and tuning parameter $\\alpha$.")

    print(xtable::xtable(table, digits = rep(4,6), # first zero "represents" row numbers which we skip later
                         align = "r|L{2cm}R{3.5cm}R{3cm}R{2cm}R{2cm}|",  # align and put a vertical line (first "l" again represents column of row numbers)
                         caption = thecaption,
                         label = paste0("perftable_check_", parameter[param])),
          add.to.row = add,
          size = "footnotesize", #Change size; useful for bigger tables "normalsize" "footnotesize"
          include.rownames = FALSE, #Don't print rownames
          include.colnames = T,
          caption.placement = "bottom", #"top","bottom", NULL
          caption.width = "\\textwidth",
          hline.after=c(-1,nrow(t)), #We don't need hline; we use booktabs -> NULL,
          floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
          sanitize.text.function = force, # Important to treat content of first column as latex function
          table.placement="H"
    )

    return(invisible(list(sample_size = N_table, density = density_table, maximal_distance = max.dist_table, tuning_parameter_alpha = qu_table)))
  }


# BRISC Method ------------------------------------------------------------

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

    # creating LaTeX table

    table = rbind(N_table, density_table, max.dist_table)
    table = as.data.frame(table[,1:6])
    table = cbind(
      grouping_paramter = c(as.character(N), as.character(density), c("1.1", "1.5", "2.0"), as.character(threshold)),
      paste0(round(table[,1], 2)," (", round(table[,2], 4),")"),
      paste0(round(table[,3], 2)," (", round(table[,4], 4),")"),
      round(table[,5], 4), round(table[,6], 4)
    )
    colnames(table) = c("grouping parameter",
                        paste0("\\eta_{", p_symbol[param], "}", " (Monte Carlo SE)"),
                        paste0("\\hat{\\eta}_{", p_symbol[param], "}", " (empirical SE)"),
                        "bias", "MSE")
    rownames(table) = NULL

    ps = c("sample size", "sample density", "maximal distance factor")
    add = list()
    add$pos = list(0,3,6)
    add$command = paste0("\\hline \n \\multicolumn{", 5, "}{|l|}{\\textbf{$\\varnothing$ grouped by ", ps, "}} \\\\ \n")
    thecaption = paste("BRISC Method - Performance measure results for the", parameter[param],"effect", "$", p_symbol[param], "$", "grouped by sample size, sample density and maximal distance factor.")

    print(xtable::xtable(table, digits = rep(4,6), # first zero "represents" row numbers which we skip later
                         align = "r|L{2cm}R{3.5cm}R{3cm}R{2cm}R{2cm}|",  # align and put a vertical line (first "l" again represents column of row numbers)
                         caption = thecaption,
                         label = paste0("perftable_brisc_", parameter[param])),
          add.to.row = add,
          size = "footnotesize", #Change size; useful for bigger tables "normalsize" "footnotesize"
          include.rownames = FALSE, #Don't print rownames
          include.colnames = T,
          caption.placement = "bottom", #"top","bottom", NULL
          caption.width = "\\textwidth",
          hline.after=c(-1,nrow(t)), #We don't need hline; we use booktabs -> NULL,
          floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
          sanitize.text.function = force, # Important to treat content of first column as latex function
          table.placement="H"
    )

    return(invisible(list(sample_size = N_table, density = density_table, maximal_distance = max.dist_table)))
  }



}
