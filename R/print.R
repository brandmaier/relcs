#' @method  print relcs.fitted
#' @S3method print relcs.fitted
print.relcs.fitted <- function(x) {
  return(paste0("Fitted RELCS model with type ",x$type,"."))
}