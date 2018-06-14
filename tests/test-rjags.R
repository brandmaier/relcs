fitRELCS.jags <- function() {
  
  
  model <- "
  model {
  for (i in 1:N){
  pi[i,1]~dnorm(sfb_mean,sfb_var)
  FS[i,1:2]~dmnorm(beta[1:2], pphi[1:2,1:2])
  X[i,1]~dnorm(mu[i,1],sigma)
  
  mu[i,1] <- FS[i,1];
  
  for (t in 2:T){
  X[i,t]~dnorm(mu[i,t], sigma)
  d[i,t-1]<-FS[i,2] + pi[i,1]*mu[i,t-1];
  mu[i,t]<-d[i,t-1] + mu[i,t-1];
  }
  }
  
  sigma~dgamma(.001,.001)
  sfb_mean ~ dnorm(0,.01)
  sfb_var~dgamma(.001,.001)
  beta[1]~dnorm(5,.01)
  beta[2]~dnorm(1,.01)
  pphi[1:2,1:2]~dwish(phi[1:2,1:2],2)
  phi[1,1]<-1
  phi[2,2]<-1
  phi[1,2]<-phi[2,1]
  phi[2,1]<-0
  
  stdsigma<-1/sigma
  std_sfb_var <- 1/sfb_var
  varphi[1:2,1:2]<-inverse(pphi[1:2,1:2])
  } # End of model
  "
  
  
  
}