#### Simstudy replication script
# corresponding to article https://doi.org/10.1007/s42519-026-00553-8


#### 0. Setup ------------------------------------------------------------------

# Required packages:
# install.packages("EgoCor")
# install.packages("BRISC")
# remotes::install_github("julia-dyck/EgoCorSim")

library(EgoCor)
library(EgoCorSim)
library(BRISC)

# All simulation files are saved to (and read from) this directory.
results_dir <- file.path(getwd(), "results_replication")
if (!dir.exists(results_dir)) dir.create(results_dir)


#### 1. True parameter values --------------------------------------------------

true.nugget   <- 60    # c_0
true.sigma_sq <- 40    # sigma_0^2
true.phi      <- 200   # phi


#### 2. Simulation design ------------------------------------------------------

# Data simulation parameters
N_values        <- c(500, 1000, 2000)
density_values  <- c(1, 4, 9)
max.dist.values <- c(457, 624, 832)

# Filter tuning parameters
threshold.factor <- c(1.1, 1.2, 1.5, 2.0, 2.5, 3.0)
qu               <- seq(from = 1, to = 0.75, by = -0.05)

# Simulation parameters
B     <- 1000  # bootstrap resamples per SE estimate
n_sim <- 3000  # simulation runs per scenario

# All 27 (N, density, max.dist) combinations
parcombi <- expand.grid(
  N            = N_values,
  nr_divisions = density_values,
  max.dist     = max.dist.values
)



#### 3. Complete realization ---------------------------------------------------
# available in EgoCorSim as object cr
head(cr)


#### 4. Run all simulation scenarios -------------------------------------------
#    Saves automatically to working directory as:
#      boot_result_<N>_<nr_divisions>_<round(max.dist,0)>_.RData

cat("Starting simulation...\n")
cat(sprintf("Scenarios: %d | n_sim: %d | B: %d\n\n",
            nrow(parcombi), n_sim, B))

old_wd <- getwd()
setwd(results_dir)

for (s in seq_len(nrow(parcombi))) {

  N_s   <- parcombi$N[s]
  den_s <- parcombi$nr_divisions[s]
  md_s  <- parcombi$max.dist[s]

  cat(sprintf("[%d/%d]  N=%d  density=%d  max.dist=%d\n",
              s, nrow(parcombi), N_s, den_s, md_s))

  tryCatch(
    EgoCorSim::one_scenario(
      input            = c(N_s, den_s, md_s),
      cr               = cr,
      nbins            = 10,
      B                = B,
      threshold.factor = threshold.factor,
      qu               = qu,
      n_sim            = n_sim
    ),
    error = function(e) {
      cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
    }
  )
  # Result saved automatically by one_scenario() as:
  # boot_result_<N_s>_<den_s>_<md_s>_.RData
}

setwd(old_wd)
cat("\nSimulation complete.\n\n")


#### 5. Performance evaluation tables ------------------------------------------

param_names <- c("nugget (c_0)", "partial sill (sigma_0^2)", "shape (phi)")

# Check-based filter
tables_check <- vector("list", 3)
for (p in 1:3) {

  tables_check[[p]] <- tryCatch(
    make_table(
      wd          = results_dir,
      param       = p,
      method      = "check",
      kick_equal0 = FALSE
    ),
    error = function(e) {
      cat("  make_table() error:", conditionMessage(e), "\n"); NULL
    }
  )
  cat("\n")
}

# Quantile-based filter
tables_quant <- vector("list", 3)
for (p in 1:3) {

  tables_quant[[p]] <- tryCatch(
    make_table(
      wd          = results_dir,
      param       = p,
      method      = "quantile",
      kick_equal0 = FALSE
    ),
    error = function(e) {
      cat("  make_table() error:", conditionMessage(e), "\n"); NULL
    }
  )
  cat("\n")
}

# BRISC comparison
tables_brisc <- vector("list", 3)
for (p in 1:3) {

  tables_brisc[[p]] <- tryCatch(
    make_table(
      wd          = results_dir,
      param       = p,
      method      = "brisc",
      kick_equal0 = FALSE
    ),
    error = function(e) {
      cat("  make_table() error:", conditionMessage(e), "\n"); NULL
    }
  )
  cat("\n")
}


#### 6. Histograms of parameter estimates and SE estimates ---------------------


# Check-based filter: one PDF per tau
for (thr in threshold.factor) {
  cat(sprintf("Histogram: check filter, tau = %.1f\n", thr))
  tryCatch(
    summarise_check_hist(
      wd          = results_dir,
      thr         = thr,
      path        = results_dir,
      filter      = c(1000, 1000, 1000),
      kick_equal0 = FALSE,
      graphic     = TRUE
    ),
    error = function(e) cat("  ERROR:", conditionMessage(e), "\n")
  )
}

# Quantile-based filter: one PDF per alpha
for (q in qu) {
  cat(sprintf("Histogram: quantile filter, qu = %.2f\n", q))
  tryCatch(
    summarise_q_hist(
      wd          = results_dir,
      qu          = q,
      path        = results_dir,
      filter      = c(1000, 1000, 1000),
      kick_equal0 = FALSE,
      graphic     = TRUE
    ),
    error = function(e) cat("  ERROR:", conditionMessage(e), "\n")
  )
}

cat(sprintf("\nHistograms saved to: %s\n", plots_dir))



## END DOC
