#### run time analysis for estimation methods

#### set working directory -----------------------------------------------------
# where results will be saved
setwd("C:/Users/jdyck/sciebo/bootstrap_uncertainty_modifications/run_time_analysis_gls_wls_apprmle")

#### load required packages ----------------------------------------------------
library(EgoCor)
library(EgoCorSim)
library(gstat)
library(BRISC)
library(peakRAM)
library(dplyr)
library(ggplot2)

#### load complete realization -------------------------------------------------
load("cr2022.RData")


#### fct for sim ---------------------------------------------------------------

# fits one semivariogram to a sample drawn from complete realization cr with chosen
# estimation method.
# Input:
# ## N: numeric, sample size
# ## nr_divisions: out of 1,4,9, determines sampling density
# ## max.dist: numeric, determines the maximal pairwise distances considered for modelling
# ## method: chr out of "gls", "wls", "aml", estimation method, either
#            generalized least squares, weighted least squares or approximate maximum likelihood
#            estimation


fit_to_one_sample = function(N, nr_divisions, max.dist, method = c("gls", "wls", "aml")){
  nbins = 10

  sample = EgoCorSim::spatial_sampling(cr, N, nr_divisions)

  if(method == "gls"){
    sp::coordinates(sample) = ~x+y

    empsv = gstat::variogram(object = sample[[1]] ~ 1, data = sample,
                             cutoff = max.dist, width = max.dist / nbins)

    ini.partial.sill = stats::var(sample[[1]])
    ini.shape = max.dist / 3
    ini.nugget = 0
    v = gstat::vgm(psill = ini.partial.sill, model = "Exp", range = ini.shape, nugget = 0)

    # Memory & time measurements
    res_mem = peakRAM({
      sv.mod = tryCatch(
        gstat::fit.variogram.gls(formula = sample[[1]] ~ 1, data = sample, model = v),
        error = function(e) {
          message("GLS estimation failed: ", e$message)
          return(NULL)
        }
      )
    })

    if (!is.null(sv.mod)) {
      est = data.frame(N = N, est.method = "gls", nugget = sv.mod$psill[1],
                       partial.sill = sv.mod$psill[2], shape = sv.mod$range[2],
                       run.time = res_mem$Elapsed_Time_sec,
                       peak_RAM_MB = res_mem$Peak_RAM_Used_MiB)
    } else {
      est = data.frame(N = N, est.method = "gls", nugget = NA,
                       partial.sill = NA, shape = NA,
                       run.time = res_mem$Elapsed_Time_sec,
                       peak_RAM_MB = res_mem$Peak_RAM_Used_MiB)
    }
  }

  if(method == "wls"){
    res_mem = peakRAM({
      sv.mod = tryCatch(
        EgoCor::vario.mod(data = sample, max.dist = max.dist, nbins = 10,
                          fit.method = 7, shinyresults = FALSE),
        error = function(e) {
          message("WLS estimation failed: ", e$message)
          return(NULL)
        }
      )
    })

    if (!is.null(sv.mod)) {
      est = data.frame(N = N, est.method = "wls", sv.mod$infotable[,4:6],
                       run.time = res_mem$Elapsed_Time_sec,
                       peak_RAM_MB = res_mem$Peak_RAM_Used_MiB)
      names(est) = c("N", "est.method", "nugget", "partial.sill", "shape", "run.time", "peak_RAM_MB")
    } else {
      est = data.frame(N = N, est.method = "wls", nugget = NA,
                       partial.sill = NA, shape = NA,
                       run.time = res_mem$Elapsed_Time_sec,
                       peak_RAM_MB = res_mem$Peak_RAM_Used_MiB)
    }
  }

  if(method == "aml"){
    coords = as.matrix(cbind(x = sample[,1], y = sample[,2]))

    res_mem = peakRAM({
      BRISC_Out = tryCatch(
        BRISC::BRISC_estimation(coords = coords, y = sample[,3]),
        error = function(e) {
          message("AML estimation failed: ", e$message)
          return(NULL)
        }
      )
    })

    if (!is.null(BRISC_Out)) {
      est = data.frame(N = N, est.method = "aml", as.list(BRISC_Out$Theta),
                       run.time = res_mem$Elapsed_Time_sec,
                       peak_RAM_MB = res_mem$Peak_RAM_Used_MiB)
      names(est) = c("N", "est.method", "nugget", "partial.sill", "shape", "run.time", "peak_RAM_MB")
    } else {
      est = data.frame(N = N, est.method = "aml", nugget = NA,
                       partial.sill = NA, shape = NA,
                       run.time = res_mem$Elapsed_Time_sec,
                       peak_RAM_MB = res_mem$Peak_RAM_Used_MiB)
    }
  }

  return(est)
}


#### testing -------------------------------------------------------------------

fit1 = fit_to_one_sample(N = 30, nr_divisions = 9, max.dist = 457, method = "gls")
fit2 = fit_to_one_sample(N = 30, nr_divisions = 9, max.dist = 457, method = "wls")
fit3 = fit_to_one_sample(N = 30, nr_divisions = 9, max.dist = 457, method = "aml")
fit1; fit2; fit3


#### run simulations -----------------------------------------------------------

# for
# a range of N,
# high sample density and with max.dist = 1.1*true practical range (ie. good conditions)
nsim = 100 # number of simulations
N = matrix(c(rep(30, nsim), rep(100, nsim), rep(500, nsim)), ncol = 1)

set.seed(43896451)

res.gls = apply(N, 1, fit_to_one_sample, nr_divisions = 9, max.dist = 457, method = "gls")
res.gls = do.call(rbind, res.gls) # combine results
res.wls = apply(N, 1, fit_to_one_sample, nr_divisions = 9, max.dist = 457, method = "wls")
res.wls = do.call(rbind, res.wls) # combine results
res.aml = apply(N, 1, fit_to_one_sample, nr_divisions = 9, max.dist = 457, method = "aml")
res.aml = do.call(rbind, res.aml) # combine results

res = rbind(res.gls, res.wls, res.aml) # combine results
save(res, file = "existing_methods_runtime_memory_simresults.RData") # save results



#### explore results -----------------------------------------------------------
load("existing_methods_estimates_and_runtimes.RData")

# summary table
summary_res <- res %>%
  group_by(est.method, N) %>%
  summarise(
    successful_est = sum(!is.na(nugget)) / n() * 100,  # percentage of successful estimates
    mean_runtime_sec = mean(as.numeric(run.time), na.rm = TRUE), # mean run time in seconds
    mean_memory_MB = mean(peak_RAM_MB, na.rm = TRUE), # mean peak RAM in MB
    mean_nugget = mean(nugget, na.rm = TRUE), # mean nugget
    mean_partial_sill = mean(partial.sill, na.rm = TRUE), # mean partial sill
    mean_shape = mean(shape, na.rm = TRUE), # mean shape
    .groups = "drop"
  )

print(summary_res)

# print latex code for table
print(xtable::xtable(summary_res, digits = c(0, 0, 0, 2, 2, 2)),
      include.rownames = FALSE,
      comment = FALSE,
      sanitize.text.function = identity)


# main insights
# ## - GLS estimation is generally slower and uses more memory than WLS and AML.



## END OF DOC
