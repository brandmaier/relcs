#' @export
fitRELCS.openmx <- function(num.obs, data, has.slope=FALSE, no.run=FALSE) {
  
  model <-  createRELCS(num.obs,has.slope = has.slope)
  model <- mxModel(model, mxData(data, type="raw"))
  
  if (no.run) {
    lcs.out <- model
  } else {
    lcs.out <- mxRun(model)
  }
  
  result <- list()
  result$fit <- lcs.out
  result$language <- "openmx"
  class(result) <- "relcs.fitted"
  
  return(result)
}