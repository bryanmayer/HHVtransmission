# Generate sufficient statistics for use in likelihood function ----

# create_likdat for marginal models, create_likdat_combined for two exposure model
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

# two exposure sources (S and M)
create_likdat_combined = function(dat){
    dat %>%
    group_by(FamilyID, infectious_1wk) %>%
    summarize(
      total_exposureM = sum(10^M), #should be 1 input for infectious_1wk == 1
      total_exposureS = sum(10^S), #should be 1 input for infectious_1wk == 1
      total_weeks = max(infant_wks)
    ) %>%
    ungroup()
}

# Survival log-likelihood functions ----

# surv_logLik for marginal/single exposure source, combined for two exposure sources
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

surv_logLik_combined = function(log_betas, likdat){

  beta0 = exp(log_betas)[1]
  betaM = exp(log_betas)[2]
  betaS = exp(log_betas)[3]

  logsurv = likdat %>% subset(infectious_1wk == 0) %>%
    mutate(calc = -total_exposureM * betaM - total_exposureS * betaS - total_weeks * beta0)

  loginf = likdat %>% subset(infectious_1wk == 1) %>%
    mutate(calc = log(1 - exp(-beta0 - total_exposureM * betaM - total_exposureS * betaS)))

  -(sum(logsurv$calc) + sum(loginf$calc))

}
