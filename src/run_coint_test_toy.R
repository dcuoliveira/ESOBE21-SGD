rm(list=ls())

library('here')
source(here('src','samplers.R'))

set.seed(4567)

#The other series
x1 <- rnorm(100, mean = 5)
x2 <- rnorm(100, mean = 7)

#Initializing y is hard
y = c(20,22)
beta <- c(2,4)
phi <- c(0.7,0.3)

#generating
#y = betaX +  phi * R_(t-1) + psi1*delta_R_(t-1)

for(i in 3:100){
  y[i] <- beta%*%as.matrix(c(x1[i],x2[i])) + 0.5*(y[(i-1)]-beta%*%as.matrix(c(x1[(i-1)],x2[(i-1)]))) + 0.3*((y[(i-1)]-beta%*%as.matrix(c(x1[(i-1)],x2[(i-1)])))-(y[(i-2)]-beta%*%as.matrix(c(x1[(i-2)],x2[(i-2)])))) + rnorm(1, 0, sd = 0.5)
}

#Using form from part 2
k <- 2

#sample size
t <- 100

npost <- 5000

Rt <- numeric(0)
for(i in 1:t){
  Rt[i] <- (y[i] - beta%*%as.matrix(c(x1[i],x2[i])))
}

sample_list_out = ARp_coint_test_gibbs_sampler(Yt=y,
                                               Xt=t(as.matrix(cbind(x1, x2))),
                                               Rt=Rt,
                                               npost=npost,
                                               k=k,
                                               t=t)
beta_list=sample_list_out$beta_list
phi_list=sample_list_out$phi_list
sigma_list=sample_list_out$sigma_list

par(mfrow = c(1,1))
boxplot(sqrt(unlist(sigma_list))[1001:5000], main = "Sigma")

par(mfrow = c(2,2))
phi1 <- numeric(0)
for(h in 1:npost){
  phi1[h] <- phi_list[[h]][1]
}
boxplot(phi1[1001:5000], main = "Phi")
quantile(phi1[1001:5000], c(0.005,0.995))


phi2 <- numeric(0)
for(h in 1:npost){
  phi2[h] <- phi_list[[h]][2]
}
boxplot(phi2[1001:5000], main = "Psi")
quantile(phi2[1001:5000], c(0.025,0.975))

beta1 <- numeric(0)
for(h in 1:npost){
  beta1[h] <- beta_list[[h]][1]
}
boxplot(beta1[1001:5000], main = "Beta_1")
quantile(beta1[1001:5000], c(0.005,0.995))

beta2 <- numeric(0)
for(h in 1:npost){
  beta2[h] <- beta_list[[h]][2]
}
boxplot(beta2[1001:5000], main = "Beta_2")
quantile(beta2[1001:5000], c(0.005,0.995))


acf(r)
pacf(r)
