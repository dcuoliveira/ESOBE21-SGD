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
T = dim(Xt)[2]
n = dim(Xt)[1]
k = 2
npost = 5000

sample_list_out = ARp_resid_coint_test_gibbs_sampler(Yt=Yt,
                                                     Xt=Xt,
                                                     Rt=Rt,
                                                     npost=npost,
                                                     n=n,
                                                     T=T,
                                                     k=k)

beta_list=sample_list_out$beta_list
rho_xi_list=sample_list_out$rho_xi_list
sigma_list=sample_list_out$sigma_list

ts.plot(Rt, main = "Model residuals")

boxplot(sqrt(unlist(sigma_list))[1001:npost], main = "Sigma")

xi <- numeric(0)
for(h in 1:npost){
  xi[h] <- rho_xi_list[[h]][2]
}
boxplot(xi[1001:npost], main = "Xi")
quantile(xi[1001:npost], c(0.025,0.975))

rho <- numeric(0)
for(h in 1:npost){
  rho[h] <- rho_xi_list[[h]][1]
}
boxplot(rho[1001:npost], main = "Rho")
quantile(rho[1001:npost], c(0.005,0.995))

