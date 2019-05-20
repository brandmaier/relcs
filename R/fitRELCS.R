#' @export
fitRELCS <- function( data, type="stan", has.slope=FALSE, ...) {
  
  num.obs <- ncol(data)
  
  if (type=="stan") {
    model <- fitRELCS.stan(data = data, num.obs = num.obs, has.slope=has.slope, ...)
  } else if (type=="openmx") {
    model <- fitRELCS.openmx(data = data, num.obs = num.obs, has.slope=has.slope, ...)
  } else {
    stop(paste0("Type",type," is not implemented, Try 'stan' or 'openmx'"))
  }
  
  return(model)
}