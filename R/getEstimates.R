#' @export
getEstimates <- function(x) {
  result <- list()
  class(result) <- "relcs.estimates"
  
  if(!inherits(x,"relcs.fitted")) {
    warning("Function can only handle relcs.fitted models")
    return(NULL)
  }
  
  if (is.null(x) | is.null(x$fit)) {
    warning("Unknown or broken object passed to getEstimates()")
        return(NULL)
  }
  
  if (x$language == "stan") {
    
    if (any(rstan::summary(x$fit)$summary[,"Rhat"]>1.1)) {
      warning("At least one Rhat value is greater than 1.1. It is recommended to increase the number of iterations.\n")
    }
    
    posterior.means <- rstan::summary(x$fit)$summary[,"mean"]
    
    # suppress output of "lp__
    if (any(names(posterior.means)=="lp__")) {
      posterior.means <- posterior.means[-which(names(posterior.means)=="lp__")]
    }
    

    result$language <- "stan"
    result$estimates <- posterior.means
    
  } else if (x$language == "openmx") {
    
    x <- omxGetParameters(x$fit)
    
    result$estimates <- x
    result$language <- "openmx"

    
  } else {
    stop("Not implemented yet!")
  }
  
  return(result)
  
}