#' Fit data and model
#' 
#' @params model OpenMx model
#' @params data a data.frame
#'
#' @export
fit <- function(model, data, type="raw") 
{
  
  model <- mxModel(model, mxData(data, type="raw"))
  result <- mxRun(model)
  #result <- mxTryHard(model)
  
  return(result)
}
