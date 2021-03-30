rm(list=ls())
library('rstanarm')
library('bayestestR')
library('bayesplot')
library('purrr')
library('insight')
library('here')
source(here('src', 'utils.R'))

data = merge_fx_sneer_data()
date = data$date
data =  data %>% select(-date)apply(data, 2, function(x) scale(x)) %>% as.data.frame()

# Bayesian normal linear regression with normal priors
bnlr = stan_glm(SGD ~ KRW + MYR + CNY + THB + IDR + TWD +
                      INR + JPY + EUR + AUD + GBP, data=data, seed=2294)
print(bnlr, digits = 3)

# Posterior of the parameters
mcmc_dens(bnlr, pars = c("KRW"))
mcmc_dens(bnlr, pars = c("MYR"))
mcmc_dens(bnlr, pars = c("CNY"))
mcmc_dens(bnlr, pars = c("THB"))
mcmc_dens(bnlr, pars = c("IDR"))
mcmc_dens(bnlr, pars = c("TWD"))
mcmc_dens(bnlr, pars = c("INR"))
mcmc_dens(bnlr, pars = c("JPY"))
mcmc_dens(bnlr, pars = c("EUR"))
mcmc_dens(bnlr, pars = c("AUD"))
mcmc_dens(bnlr, pars = c("GBP"))

# Full description of the model parameters
describe_posterior(bnlr)

# MaximuM a posterior (MAP) estimate 
post = get_parameters(bnlr)
print(map_dbl(post, map_estimate),digits = 3)

plot(date, bnlr$residuals, type = 'l', col = 'red')
par(new = 'T')
plot(date, bnlr$fitted.values, type = 'l', col = 'black')
par(new = 'T')
plot(date, data$SGD, type = 'l', col = 'blue')
PP.test(bnlr$residuals)

# Highest density interval
hdi(bnlr)

