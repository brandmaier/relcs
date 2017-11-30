#' Simulate Data from RELCS model
#'
#'
#' @export
simulateDataFromRELCS <- function(N, num.obs, autoregressionmean=.3, autoregressionvariance=.1, residualerrorvariance=.1, 
                               slopevariance=0.1, interceptvariance=0.1,has.slope=TRUE, has.icept=TRUE) {
  
  data <- matrix(NA, nrow=N, ncol=num.obs)
  population.model <- createDCSM(num.obs = num.obs,
                                 autoregression = autoregressionmean,
                                 residualerrorvariance = residualerrorvariance,
                                 slopevariance = slopevariance,
                                 has.slope = has.slope,has.icept = has.icept)
  
  for (i in 1:N) {
    ar <- rnorm(1,mean = autoregressionmean, sd = sqrt(autoregressionvariance))
    
    population.model <- omxSetParameters(population.model,labels = "ar_mu",values = ar,free = TRUE)
    
    data[i,] <- simulateData(population.model, 1)
  }
  
  data <- data.frame(data)
  names(data) <- paste0("X",1:num.obs)
  
  return(data)
}
