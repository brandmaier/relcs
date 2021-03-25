#' @export
get_beta_estimates <- function(x) {
  sm <- rstan::summary(x$fit)$summary[,"mean"]
  beta_values <- sm[startsWith(names(sm),"beta")]
  
  return(beta_values)
}