# Wrappers for the optim functions ----

# the fitters are similar except marginal expects initials to be a n X 2 matrix and combined expects initials to be a n X 3 matrix. Initials are randomly generated initial values
# likdat is processed data from create_likdat (risk_fit_functions.R)
# fitfun is the appropriate survival likelihood function (risk_fit_functions.R)
# get_best_fit() is below

marginal_fitter = function(likdat, initials, fitfun = surv_logLik){

  if(!is.matrix(initials)) initials = matrix(initials, ncol = 2)

  fits_mods = map2(initials[,1], initials[,2],
                   ~optim(c(.x, .y), fitfun, likdat = likdat))
  fit_modsBFGS = map2(initials[1], initials[2],
                      ~optim(c(.x, .y), fitfun, likdat = likdat, method = "BFGS"))
  get_best_fit(fits_mods, fit_modsBFGS)

}

combined_fitter = function(likdat, initials, fitfun = surv_logLik_combined){

  fits_mods = pmap(as.data.frame(initials),
                   ~optim(c(..1, ..2, ..3), fitfun, likdat = likdat))
  fit_modsBFGS = pmap(as.data.frame(initials),
                   ~optim(c(..1, ..2, ..3), fitfun, likdat = likdat,
                          method = "BFGS"))

  get_best_fit(fits_mods, fit_modsBFGS)

}

# get_best_fit traverses the model results to find the smallest log likelihood
# which.min chooses the first minimum (effectively random as initials were generated randomly)
# BFGS is chosen of NM for ties because BFGS will find boundary (zero) fits

get_best_fit = function(fits_mods, fit_modsBFGS){
  loglik = map(fits_mods, ~(.x$value)) %>% flatten_dbl()
  loglikBFGS = map(fit_modsBFGS, ~(.x$value)) %>% flatten_dbl()

  best_fitNM =  which.min(loglik)
  best_fitBFGS = which.min(loglikBFGS)

  if(loglikBFGS[best_fitBFGS] <= loglik[best_fitNM]){
    final_model = fit_modsBFGS[[best_fitBFGS]]
    final_model$method = "BFGS"
  } else {
    final_model = fits_mods[[best_fitNM]]
    final_model$method = "NM"
  }

  final_model
}


## Tidy processing functions ----

# Wrappers to convert optim mode output into tidy data

tidy_fits = function(lik_res){
  tibble(
    beta0 = exp(lik_res$par[1]),
    betaE = exp(lik_res$par[2]),
    loglik = lik_res$value,
    method = lik_res$method
  )
}

tidy_fits_combined = function(lik_res){
  tibble(
    beta0 = exp(lik_res$par[1]),
    betaM = exp(lik_res$par[2]),
    betaS = exp(lik_res$par[3]),
    loglik = lik_res$value,
    method = lik_res$method
  )
}

