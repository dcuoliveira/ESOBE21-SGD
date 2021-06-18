rm(list=ls())
library('here')
library('dplyr')
library('rstanarm')
library('LaplacesDemon')
library('numDeriv')
source(here('src', 'utils.R'))

data = merge_fx_sneer_data()
data_orig = data %>% select(-date)
date = data$date
data =  data %>% select(-date) %>% apply(2, function(x) scale(x)) %>% as.data.frame()

# Bayesian normal linear regression with normal priors
bnlr = stan_glm(SGD ~ KRW + MYR + CNY + THB + IDR + TWD +
                  INR + JPY + EUR + AUD + GBP, data=data_orig, seed=2294)
Rt = bnlr$residuals

# Priors
T = 100
sigma2 = var(Rt)

# Conditional marginal likelihood under the unit root process
phi_h0 = 1
epsilon_h0 = 1 - phi_h0

# We need to make sure that the lowest order term in h2 has order less than or 
# equal to the lowest order term in h1


cond_marg_like_phi1 = numDeriv(sqrt(h1_epsilon_poly / h2_epsilon_poly)




