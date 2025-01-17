#' Semi-variogram parameter uncertainty with filtered bootstrapping
#'
#' Standard error estimates for the nugget effect \eqn{c_0},  partial sill \eqn{\sigma_0^2} and
#' shape parameter \eqn{\phi} of a fitted exponential semi-variogram model.
#'
#' @param vario.mod.output An output of the \code{vario.mod} function containing the
#'                         information of the estimated exponential semi-variogram model of interest.
#' @param mod.nr The index number specifiying one of the exponential semi-variogram models
#'               listed in the \code{vario.mod.ouput}.
#' @param par.est A vector of length three containing the estimated parameters:
#'                the nugget effect, the partial sill and the shape parameter
#'                of the estimated exponential semi-variogram model.
#'                It is automatically extracted from the \code{vario.mod.output}, if provided.
#' @param data The data frame or matrix used to estimate the exponential semi-variogram of interest
#'             containing the x-coordinates in meters in the first column,
#'             the y-coordinates in meters in the second column and the data values in the third column.
#'             It is automatically extracted from the vario.mod.output, if provided.
#' @param max.dist The maximal distance used for the estimation of the
#'                 exponential semi-variogram model of interest.
#'                It is automatically extracted from the vario.mod.output, if provided.
#' @param nbins The number of bins used for the estimation of the exponential
#'              semi-variogram model of interest.
#'              It is automatically extracted from the vario.mod.output, if provided.
#' @param B The number of bootstrap repetitions to generate a set of re-estimates
#'          of each parameter.
#' @param threshold.factor The threshold factor specifies the filter within the filtered
#'                         bootstrap method (see details). If not specified, a default value of 1.2 is used.
#' @param fit.method Gstat fit method that is used
#'
#' @details \strong{Two alternative approaches for the input of the arguments:}
#'
#'          1. Provide the arguments
#'          vario.mod.output (output object from vario.mod function) and
#'          mod.nr (number of the model in the infotable).
#'
#'          2. Provide the necessary information manually, namely
#'          \code{par.est} (vector with estimated nugget, partial sill and shape parameters),
#'          \code{data} (used to estimate the semi-variogram model parameters),
#'          \code{max.dist} (semi-variogram parameter, numeric of length 1) and
#'          \code{nbins} (semi-variogram parameter, numeric of length 1).
#'
#'
#'          \strong{Filtered bootstrap method}:
#'
#'          For the semi-variogram model parameter estimation, the weighted least squares method is used
#'          in order to make the numerical calculation possible for large sample sizes.
#'          A filter is set up within the bootstrapping process to remove all
#'          bootstrap estimates for which the estimation algorithm for the semi-variogram
#'          model parameters did not converge.
#'
#'          The parameter standard errors are estimated using the generalized bootstrap
#'          method with check-based filtering.
#'          The semi-variogram structure from the given model is used to remove the
#'          spatial correlation structure within the original dataset. Then,
#'          classical bootstrap sampling with replacement is used to generate B
#'          bootstrap samples from the uncorrelated data.
#'          Each bootstrap sample inherits the correlation structure back and is used to estimate
#'          the nugget effect, partial sill and shape parameter for an exponential model.
#'          Within the bootstrap repetitions, a test is performed to check whether
#'          the estimated parameters lie within a probable range.
#'          If the total variance of the bootstrap model exceeds the empirical variance
#'          of the data times the treshold factor \eqn{\tau}, ie.
#'          \deqn{c_{0 b}^* + \sigma_{0 b}^{2*} > \tau  \widehat{Var(\mathbf{z})}}
#'          for the bth bootstrap estimate, it is discarded. Otherwise, it is saved.
#'          This procedure is performed until B bootstrap estimates have aggregated.
#'          The empirical standard deviation calculated from the bootstrap estimates provides the
#'          uncertainty estimate for each parameter.
#'
#'          Details about the algorithm used to obtain standard errors for the parameters
#'          of the exponential semi-variogram model are provided in \insertCite{dyck_sv_ses;textual}{EgoCor}.
#'
#'          \strong{Reproducibility}:
#'
#'          In order to generate reproducible bootstrap results, set a random seed with the command \code{set.seed()}
#'          before using the \code{par.uncertainty} function.
#'
#' @return
#' The function returns parameter estimates and corresponding standard error estimates
#' together and provides a list with the following objects:
#' \item{se}{A vector of length 3 containing the estimated standard errors of the
#'           nugget effect, the partial sill and the shape parameter.}
#' \item{unc.table}{A matrix containing the parameter estimates and the corresponding standard errors.}
#' \item{re_estimates}{A matrix with B rows containing the set of bootstrap re-estimates for each parameter.}
#' \item{re_estimate.mean}{A vector containing the mean parameter estimates based on the set of bootstrap re-estimates for each parameter.}
#' \item{call}{The function call.}

#' @references
#'   \insertAllCited{}
#'
#' @export



par_uncertainty_check = function(vario.mod.output, mod.nr,
                           par.est = NULL, data= NULL, max.dist=NULL,nbins=NULL,
                           B = 1000, threshold.factor = c(1.1, 1.5, 2.0, 2.5, 3.0),
                           fit.method = 7){

  unc = EgoCor::par.uncertainty2(vario.mod.output, mod.nr, par.est, data, max.dist, nbins,
                                 B, threshold.factor, mc.cores = 1)
  est = c(unc$unc.table[1:3,1]$nugget, unc$unc.table[1:3,1]$partial.sill, unc$unc.table[1:3,1]$shape)
  names(est) = c("nugget", "partial.sill", "shape")
  sds = unc$se
  nr_reest = unc$nr_reest

  result = numeric(0)
  for (i in 1:length(threshold.factor)){
    result_i = c(sds[(i-1)*3 + 1:3], nr_reest[1,1], nr_reest[i,2], nr_reest[i,3])
    names(result_i) = paste0(c("n.sd.check.", "ps.sd.check.", "s.sd.check.", "nr_reest_gstat", "nr_reest_thr", "nr_overlap"), threshold.factor)
    result = c(result, result_i)
  }
  return(c(est, result))
}







