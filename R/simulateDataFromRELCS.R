#' Simulate Data from RELCS model
#'
#'
#' @export
simulateDataFromRELCS <- function(N, num.obs, autoregressionmean=.3, 
                                  autoregressionvariance=.1, 
                                  residualerrorvariance=.1, 
                                  slopevariance=0.0, slopemu=0, interceptvariance=0.1, interceptmu=4,
                                  has.slope=TRUE, has.icept=TRUE, use.openmx=FALSE) {
  

  if (use.openmx) {
  
  data <- matrix(NA, nrow=N, ncol=num.obs)
  population.model <- createLCS(num.obs = num.obs,
                                 autoregression = autoregressionmean,
                                 residualerrorvariance = residualerrorvariance,
                                 slopevariance = slopevariance,
                                slopemu = slopemu, interceptmu = interceptmu,
                                interceptvariance = interceptvariance,
                                 has.slope = has.slope,has.icept = has.icept)
  
  for (i in 1:N) {
    sfb <- rnorm(1,mean = autoregressionmean, sd = sqrt(autoregressionvariance))
    
    population.model <- omxSetParameters(population.model,
                                         labels = pkg.globals$SELF_FEEDBACK_FE,
                                         values = sfb,free = TRUE)
    
    data[i,] <- simulateData(population.model, 1)
  }
  

  
  } else {
    
    if (has.slope) stop("Not implemented!")
    
    # parameters
    beta_0 <- interceptmu
    
    phi2_0 <- interceptvariance
    sigma2_u <- residualerrorvariance
    times <- num.obs

    data <- matrix(NA,N,times)
    ly <- rep(NA,times)
    dy <- rep(NA,times-1)
    
    
    
    for(i in 1:N){
      
      b_0i <- beta_0 + sqrt(phi2_0)*rnorm(1)
      
      for(t in 1:times){
        if(t == 1){
          ly[t] <- b_0i
        }else{
          dy[t-1] <- rnorm(1,autoregressionmean,sqrt(autoregressionvariance))*ly[t-1]# + b_1i
          ly[t] <- ly[t-1] + dy[t-1]
        }
        data[i,t] <- ly[t] + sqrt(sigma2_u)*rnorm(1)
      }
    }
    
    
  }
  
  data <- data.frame(data)
  names(data) <- paste0("X",1:num.obs)
  
  return(data)
}
