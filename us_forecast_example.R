rm(list=ls())
library('mfbvar')
data("mf_usa")

# Quarterly: GDPC1
# Monthly: UNRATE, CPIAUCSL

## Set priors:
### 1. set_rior
mfbvar_prior = set_prior(Y = mf_usa, 
                         n_lags = 4,
                         n_reps = 1000)
### 2. update_prior
prior_intervals = matrix(c(1, 3, 4, 8, 1, 3), 
                         ncol = 2, 
                         byrow = TRUE)
moments = interval_to_moments(prior_intervals)
mfbvar_prior = update_prior(mfbvar_prior,
                            d = "intercept",
                            prior_psi_mean = moments$prior_psi_mean,
                            prior_psi_Omega = moments$prior_psi_Omega)
plot(mfbvar_prior)
mfbvar_prior = update_prior(mfbvar_prior,
                            n_fcst = 24)
summary(mfbvar_prior)

## Compute posteriors:
### 1. estimate_mfbvar
mod_ss_iw = estimate_mfbvar(pmfbvar_prior = mfbvar_prior,
                            prior = "ss",
                            variance = "iw")
mod_ssng_iw = estimate_mfbvar(mfbvar_prior = mfbvar_prior, 
                              prior = "ssng", 
                              variance = "iw")

## Process output: 
### 1. plot-mfbvar
### 2. varplot
### 3. predict.mfbvar


