rm(list=ls())
library('rstanarm')
library('here')
library('dplyr')
library('mvtnorm')
source(here('src', 'utils.R'))

## DATA
data = merge_fx_sneer_data()
data_orig = data %>% select(-date)
date = data$date
data =  data %>% select(-date) %>% apply(2, function(x) scale(x)) %>% as.data.frame()

## HYPERPARAMETERS
n = 100
k = 1

## BAYESIAN NORMAL LINEAR REGRESSION WITH NORMAL PRIORS
bnlr = stan_glm(SGD ~ KRW + MYR + CNY + THB + IDR + TWD +
                  INR + JPY + EUR + AUD + GBP, data=data_orig, seed=2294)

# Residuals of the BNLR
R_t = as.array(bnlr$residuals)

# Reponse variable
Y_t = as.array(data$SGD)

## TARGET CONDITIONAL DISTRIBUTIONS

# data
X_rho_xi = gen_X_rho_xi(R_t=R_t,
                        k=k)
Y_rho_xi = gen_Y_rho_xi(Y_t=Y_t,
                        k=k)

# parameters
sigma2 = 1

# rho, xi | data, sigma2, beta
cond_dist_rho_xi = rmvnorm(n=n,
                           mean=solve(X_rho_xi %*% t(X_rho_xi)) %*% X_rho_xi %*% Y_rho_xi,
                           sigma=sigma2 * solve(X_rho_xi %*% t(X_rho_xi)))



