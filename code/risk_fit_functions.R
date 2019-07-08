# Generate sufficient statistics for use in likelihood function
# dat: data for optimization formatted per this analysis,
## requires infant_wks, infectious_1wk, exposure (10^count), FamilyID
## output is input data for two components of survival likelihood
### (infectious_1wk = 0: survival input; infectious_1wk = 1: infection interval input)

create_likdat = function(dat){
    dat %>%
    group_by(FamilyID, infectious_1wk) %>%
    summarize(
      total_exposure = sum(exposure), #should be 1 input for infectious_1wk == 1
      total_weeks = max(infant_wks)
    ) %>%
    ungroup()
}


# Survival log-likelihood function
# log_betas: input vector of logged betas (log beta0, log betaE) for fitting
# dat: lik data for optimization formatted per this analysis,
## requires infectious_1wk (surv_indicator), total_exposure, total_weeks, FamilyID

surv_logLik = function(log_betas, likdat){
  beta0 = exp(log_betas)[1]
  betaE = exp(log_betas)[2]

  logsurv = likdat %>% subset(infectious_1wk == 0) %>%
    mutate(calc = -total_exposure * betaE - total_weeks * beta0)

  loginf = likdat %>% subset(infectious_1wk == 1) %>%
    mutate(calc = log(1 - exp(-beta0 - betaE * total_exposure)))

  -(sum(logsurv$calc) + sum(loginf$calc))

}

# two exposure sources (S and M)
create_likdat2 = function(dat){
    dat %>%
    group_by(FamilyID, infectious_1wk) %>%
    summarize(
      total_exposureM = sum(10^M), #should be 1 input for infectious_1wk == 1
      total_exposureS = sum(10^S), #should be 1 input for infectious_1wk == 1
      total_weeks = max(infant_wks)
    ) %>%
    ungroup()
}

surv_logLik_2E = function(log_betas, likdat){

  beta0 = exp(log_betas)[1]
  betaM = exp(log_betas)[2]
  betaS = exp(log_betas)[3]

  logsurv = likdat %>% subset(infectious_1wk == 0) %>%
    mutate(calc = -total_exposureM * betaM - total_exposureS * betaS - total_weeks * beta0)

  loginf = likdat %>% subset(infectious_1wk == 1) %>%
    mutate(calc = log(1 - exp(-beta0 - total_exposureM * betaM - total_exposureS * betaS)))

  -(sum(logsurv$calc) + sum(loginf$calc))

}

tidy_fits2 = function(lik_res){
  tibble(
    beta0 = exp(lik_res$par[1]),
    betaM = exp(lik_res$par[2]),
    betaS = exp(lik_res$par[3]),
    loglik = lik_res$value#,
    #loglik_calc = surv_logLik(fit_2parm$par, fit_data)
  )
}

# likelihood wrapper for use in optimizer when betaE = 0
surv_logLik0 = function(x0, likdat) surv_logLik(c(x0, -Inf), likdat)

# likelihood calculation for beta0 model (constant risk)
exact_loglik0 = function(beta0, likdat) {
  beta0 * sum(dat$surv_weeks) - log(1-exp(-beta0)) * sum(dat$infected)
}

beta0_mle = function(likdat){
  total_wks = sum(subset(likdat, infectious_1wk == 1)$total_weeks)
  -log(1 - n_distinct(likdat$FamilyID)/total_wks)
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


beta0_calc = function(risk) -log(1 - risk)
risk0_calc = function(beta0) 1 - exp(-beta0)

