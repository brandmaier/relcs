#' Simulate Data from RELCS model
#'
#'
#' @export
simulateDataFromRELCS <- function(N, num.obs, autoregressionmean=.3, 
                                  autoregressionvariance=.1, 
                                  residualerrorvariance=.1, 
                                  slopevariance=0.1, slopemu=1, interceptvariance=0.1, interceptmu=4,
                                  has.slope=TRUE, has.icept=TRUE) {
  

  
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
  
  data <- data.frame(data)
  names(data) <- paste0("X",1:num.obs)
  
  return(data)
}
