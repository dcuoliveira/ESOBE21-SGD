library('here')
source(here('src/utils.R'))

ARp_resid_coint_test_gibbs_sampler = function(Yt,
                                              Xt,
                                              Rt,
                                              npost,
                                              n,
                                              T,
                                              k){

  beta_list <- vector("list", npost)
  sigma_list <- vector("list", npost)
  rho_xi_list <- vector("list", npost)
  print(" Running ARp residual-based cointegration test")
  pb = txtProgressBar(min = 0,
                      max = npost,
                      style = 3)
  # Gibbs sampler
  for(h in 1:npost){
    if(h == 1){
      #sigma
      sigma2 <- 1
      #Initializing beta
      beta <- t(as.matrix(c(1,1)))  
    }else{
      sigma2 <- sigma_post
      beta <- beta_post
    }
    delta_Rt = t(c(NA, diff(Rt)))
    X_rho_xi = gen_X_rho_xi(Rt=Rt,
                            delta_Rt=delta_Rt,
                            T=T,
                            k=k)
    Y_rho_xi <- as.matrix(Rt[(k+1):T])
    
    #Sample rho and xi
    rho_xi_post <- MASS::mvrnorm(n = 1,
                              mu = solve(X_rho_xi%*%t(X_rho_xi)) %*% X_rho_xi %*% Y_rho_xi,
                              Sigma = sigma2 * solve(X_rho_xi %*% t(X_rho_xi)))

    delta_Xt <- t(rbind(rep(0, n), diff(t(Xt))))
    X_beta2 = gen_X_beta2(rho_xi_post=rho_xi_post,
                           Xt=Xt,
                           delta_Xt=delta_Xt,
                           T=T,
                           k=k,
                           n=n)

    delta_Yt <- t(c(NA, diff(Yt)))
    Y_beta2 = gen_Y_rho_psi(rho_xi_post=rho_xi_post,
                            Yt=Yt,
                            delta_Yt=delta_Yt,
                            T=T,
                            k=k)
    #Sample beta
    beta_post <- MASS::mvrnorm(n = 1,
                               mu = solve(X_beta2 %*% t(X_beta2)) %*% X_beta2 %*% t(Y_beta2),
                               Sigma = sigma2 * solve(X_beta2 %*% t(X_beta2)))
    
    #sample sigma
    #Update r
    Rt <- numeric(0)
    for(i in 1:T){
      Rt[i] <- (Yt[i] - beta_post %*% Xt[,i])
    }
    
    delta_Rt = t(c(NA, diff(Rt)))
    
    Rt_tau <- numeric(0)
    for(i in (T):(k+1)){
      aux <- rho_xi_post[1]*Rt[i]
      for(j in 1:(k-1)){
        aux <-  aux + rho_xi_post[(j+1)]* delta_Rt[(i-j)]
      }
      
      Rt_tau[i] <- Rt[i]- aux
    }
    
    nu <- T-k
    tau2 <- (1/(T-k)) * sum(Rt_tau^2, na.rm = TRUE)
    sigma_post <- LaplacesDemon::rinvchisq(1, df = nu, scale=tau2)
    
    beta_list[[h]] <- beta_post
    rho_xi_list[[h]] <- rho_xi_post
    sigma_list[[h]] <- sigma_post
    
    setTxtProgressBar(pb, h)
    
  }
  return(list(beta_list=beta_list,
              rho_xi_list=rho_xi_list,
              sigma_list=sigma_list))
}