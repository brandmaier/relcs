#' @export
fitRELCS.openmx <- function(num.obs, data) {
  
  model <-  createRELCS(num.obs)
  model <- mxModel(model, mxData(data, type="raw"))
  lcs.out <- mxRun(model)
  
  result <- list()
  result$fit <- lcs.out
  result$language <- "openmx"
  class(result) <- "relcs.fitted"
  
  return(result)
}