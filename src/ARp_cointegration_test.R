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

X_t = data %>% select(-SGD) %>% as.matrix()
Y_t = data %>% select(SGD) %>% as.matrix()

## HYPERPARAMETERS
m = 100
n = dim(data)[2] - 1
k = 1

## BAYESIAN NORMAL LINEAR REGRESSION WITH NORMAL PRIORS
bnlr = stan_glm(SGD ~ KRW + MYR + CNY + THB + IDR + TWD +
                  INR + JPY + EUR + AUD + GBP, data=data_orig, seed=2294)

# Residuals of the BNLR
R_t = as.array(bnlr$residuals)

## TARGET CONDITIONAL DISTRIBUTIONS

# priors
sigma2 = 1

# rho, xi | data, sigma2, beta
X_rho_xi = gen_X_rho_xi(R_t=R_t,
                        k=k)
Y_rho_xi = gen_Y_rho_xi(Y_t=Y_t,
                        k=k)
cond_dist_rho_xi = rmvnorm(n=m,
                           mean=solve(X_rho_xi %*% t(X_rho_xi)) %*% X_rho_xi %*% Y_rho_xi,
                           sigma=sigma2 * solve(X_rho_xi %*% t(X_rho_xi)))

# beta_2 | data, rho, xi, sigma2
X_beta_2 = gen_X_beta_2(X_t=X_t,
                        n=n,
                        k=k, 
                        rho=NULL,
                        xi=NULL)
Y_beta_2 = gen_Y_beta_2(X_t=X_t,
                        n=n,
                        k=k)
cond_dist_beta_2 = rmvnorm(n=m,
                           mean=solve(X_beta_2 %*% t(X_beta_2)) %*% X_beta_2 %*% Y_beta_2,
                           sigma=sigma2 * solve(X_beta_2 %*% t(X_beta_2)) )






