rm(list=ls())
library('rstanarm')
library('here')
library('dplyr')
source(here('src', 'utils.R')) # merge_fx_sneer_data
source(here('src', 'samplers.R')) # ARp_resid_coint_test_gibbs_sampler

data = merge_fx_sneer_data()
data_orig = data %>% select(-date)
date = data$date
data =  data %>% select(-date) %>% apply(2, function(x) scale(x)) %>% as.data.frame()

# Bayesian normal linear regression with normal priors
bnlr = stan_glm(SGD ~ KRW + MYR + CNY + THB + IDR + TWD +
                  INR + JPY + EUR + AUD + GBP, data=data_orig, seed=2294)

# Run residual-based cointegration test based on the work of Furmston, Hailes and Morton (2013)
Yt = bnlr$data$SGD
Xt = bnlr$data %>% select(KRW, MYR, CNY, THB, IDR, TWD, INR, JPY, EUR, AUD, GBP) %>% t()
Rt = bnlr$residuals
t = dim(Xt)[2]
k = dim(Xt)[1]
npost = 5000

sample_list_out = ARp_resid_coint_test_gibbs_sampler(Yt=Yt,
                                                     Xt=Xt,
                                                     Rt=Rt,
                                                     npost=npost,
                                                     k=k,
                                                     t=t)



