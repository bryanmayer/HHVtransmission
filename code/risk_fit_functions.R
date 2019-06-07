
# Survival log-likelihood function
# log_betas: input vector of logged betas (log beta0, log betaE) for fitting
# dat: data for optimization formatted per this analysis,
## requires infant_wks, infectious_1wk, exposure (10^count), FamilyID

surv_logLik = function(log_betas, dat){
  beta0 = exp(log_betas)[1]
  betaE = exp(log_betas)[2]

  likdata = dat %>%
    group_by(FamilyID, infectious_1wk) %>%
    summarize(
      total_exposure = sum(exposure), #should be 1 input for infectious_1wk == 1
      total_weeks = max(infant_wks)
    )

  logsurv = likdata %>% subset(infectious_1wk == 0) %>%
    mutate(calc = -total_exposure * betaE - total_weeks * beta0)

  loginf = likdata %>% subset(infectious_1wk == 1) %>%
    mutate(calc = log(1 - exp(-beta0 - betaE * total_exposure)))

  -(sum(logsurv$calc) + sum(loginf$calc))

}

# Tidy likelihood fit
tidy_fits = function(lik_res){
  tibble(
    beta0 = exp(lik_res$par[1]),
    betaE = exp(lik_res$par[2]),
    loglik = lik_res$value#,
    #loglik_calc = surv_logLik(fit_2parm$par, fit_data)
  )
}


