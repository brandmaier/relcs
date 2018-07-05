getEstimates <- function(x) {
  
  if (x$language == "stan") {
    
    posterior.means <- summary(x$fit)$summary[,"mean"]
    
    return(posterior.means)
    
  } else if (x$language == "openmx") {
    
    return(omxGetParameters(x$fit))
    
  } else {
    stop("Not implemented yet!")
  }
  
}