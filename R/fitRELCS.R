fitRELCS <- function(type, num.obs, data, ...) {
  if (type=="stan") {
    fitRELCS.stan(data = data, num.obs = num.obs)
  } else if (type=="lavaan") {
    fitRELCS.lavaan(data = data, num.obs = num.obs)
  } else {
    stop("Not implemented yet")
  }
}