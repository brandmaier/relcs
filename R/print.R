#' @method  print relcs.fitted
#' @S3method print relcs.fitted
print.relcs.fitted <- function(x) {
  print(paste0("Fitted RELCS model of type ",x$type,"."))
}