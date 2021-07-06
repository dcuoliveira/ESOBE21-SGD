set.seed(4567)

#The other series
x1 <- rnorm(100, mean = 5)
x2 <- rnorm(100, mean = 7)


#Initializing y is hard
y = c(40,22)
beta <- c(2,4)
phi <- c(0.7,0.3)

#generating
#y = betaX +  phi * R_(t-1) + psi1*delta_R_(t-1)

for(i in 3:100){
  y[i] <- beta%*%as.matrix(c(x1[i],x2[i])) + 0.7*(y[(i-1)]-beta%*%as.matrix(c(x1[(i-1)],x2[(i-1)]))) + 0.3*((y[(i-1)]-beta%*%as.matrix(c(x1[(i-1)],x2[(i-1)])))-(y[(i-2)]-beta%*%as.matrix(c(x1[(i-2)],x2[(i-2)])))) + rnorm(1, 0, sd = 0.5)
}

par(mfrow=c(1,1))

plot.ts(y[1:100])
plot.ts(x1[1:100])
plot.ts(x2[1:100])


#Using form from part 2
k <- 2

#sample size
t <- 100

npost <- 1000




beta_list <- vector("list", npost)
sigma_list <- vector("list", npost)
phi_list <- vector("list", npost)

for(h in 1:npost){
  if(h == 1){
    #sigma
    sigma2 <- 0.5
    #Initializing beta
    beta <- t(as.matrix(c(2,4)))  
  }else{
    sigma2 <- sigma_post
    beta <- beta_post
  }
  
  
  
  r <- numeric(0)
  for(i in 1:t){
    r[i] <- (y[i] - beta%*%as.matrix(c(x1[i],x2[i])))
  }
  
  delta_r <- numeric(0)
  for(i in 2:t){
    delta_r[i] <- r[i]-r[(i-1)]
  }
  
  #Creating X matrix
  
  x_vec <- c(r[k:(t-1)])
  for(i in k:2){
    aux <- delta_r[i:(t+1-i)]
    x_vec <- c(x_vec,aux)
  }
  
  X <- matrix(x_vec, nrow = k, byrow = T)
  Y <- as.matrix(r[(k+1):t])
  
  
  #Sample rho and psi
  phi_post <- MASS::mvrnorm(n = 1, mu = solve(X%*%t(X))%*%X%*%Y, Sigma = sigma2*solve(X%*%t(X)))
  
  
  
  X_beta <- matrix(c(x1,x2),nrow=2, byrow = T)
  X_beta_delta <- matrix(0, nrow = 2, ncol = t)
  for(i in 2:t){
    X_beta_delta[,i] = X_beta[,i] - X_beta[,(i-1)]
  }
  
  
  x_beta2 <- matrix(0, nrow = 2, ncol = (t-k))
  for(i in 1:(t-k)){
    aux <- 0
    
    aux <- phi_post[1]*X_beta[,(i+k-1)]
    for(j in 1:(k-1)){
      aux <-  aux + phi_post[(j+1)]*X_beta_delta[,(i+k-j)]
    }
    
    x_beta2[,i] <- aux
  }
  
  
  
  y_beta_delta <- matrix(0, nrow = 1, ncol = t)
  for(i in 2:t){
    y_beta_delta[,i] = y[i] - y[(i-1)]
  }
  
  y_beta2 <- matrix(0, nrow = 1, ncol = (t-k))
  for(i in 1:(t-k)){
    aux <- 0
    
    aux <- phi_post[1]*y[(i+k-1)]
    for(j in 1:(k-1)){
      aux <-  aux + phi_post[(j+1)]*y_beta_delta[(i+k-j)]
    }
    
    y_beta2[,i] <- aux
  }
  
  
  #Sample beta
  beta_post <- MASS::mvrnorm(n = 1, mu = solve(x_beta2%*%t(x_beta2))%*%x_beta2%*%t(y_beta2), Sigma = sigma2*solve(x_beta2%*%t(x_beta2)))
  
  
  #sample sigma
  #Update r
  r <- numeric(0)
  for(i in 1:t){
    r[i] <- (y[i] - beta_post%*%as.matrix(c(x1[i],x2[i])))
  }
  
  r_delta <- matrix(0, nrow = 1, ncol = t)
  for(i in t:2){
    r_delta[,i] = r[i] - r[(i-1)]
  }
  
  r_tau <- numeric(0)
  for(i in (t):(k+1)){
    aux <- phi_post[1]*r[i]
    for(j in 1:(k-1)){
      aux <-  aux + phi_post[(j+1)]*r_delta[(i-j)]
    }
    
    r_tau[i] <- r[i]- aux
  }
  
  nu <- t-k
  tau2 <- (1/(t-k))*sum(r_tau^2, na.rm = T)
  
  
  sigma_post <- LaplacesDemon::rinvchisq(1, df = nu, scale=tau2)
  
  beta_list[[h]] <- beta_post
  phi_list[[h]] <- phi_post
  sigma_list[[h]] <- sigma_post

}


par(mfrow = c(1,1))
boxplot(sqrt(unlist(sigma_list)), main = "Sigma")


par(mfrow = c(2,2))
phi1 <- numeric(0)
for(h in 1:npost){
  phi1[h] <- phi_list[[h]][1]
}
boxplot(phi1, main = "Phi")

phi2 <- numeric(0)
for(h in 1:npost){
  phi2[h] <- phi_list[[h]][2]
}
boxplot(phi2, main = "Psi")


beta1 <- numeric(0)
for(h in 1:npost){
  beta1[h] <- beta_list[[h]][1]
}
boxplot(beta1, main = "Beta_1")

beta2 <- numeric(0)
for(h in 1:npost){
  beta2[h] <- beta_list[[h]][2]
}
boxplot(beta2, main = "Beta_2")

