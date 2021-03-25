#' @export
get_beta_estimates <- function(x, statistic="mean") {
  
  sm <- rstan::summary(x$fit)$summary
  
  if (!(statistic %in% colnames(sm) )) {
    warning(paste0("Please choose a valid statistic such as ", paste0(colnames(sm),collapse=", ")))
    return(NA)
  }
  
  sm <- sm[, statistic]
  beta_values <- sm[startsWith(names(sm),"beta")]
  
  return(beta_values)
}
