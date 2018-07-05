#' @export
fitRELCS <- function( data, type="stan", has.slope=FALSE, ...) {
  
  num.obs <- ncol(data)
  
  if (type=="stan") {
    model <- fitRELCS.stan(data = data, num.obs = num.obs, has.slope=has.slope, ...)
  } else if (type=="lavaan") {
    model <- fitRELCS.lavaan(data = data, num.obs = num.obs, has.slope=has.slope, ...)
  } else {
    stop("Not implemented yet")
  }
  
  return(model)
}