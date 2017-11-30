#' Simulate Data
#' @description simulate data from model implied distribution
#' 
#' @params OpenMx model
#' @params N sample size
#' 
#' @export
simulateData <- function (model, N) 
{
  run <- model
  fit <- NULL
  data <- NULL
  manifests <- run@manifestVars
  fake.data <- data.frame(matrix(1:(length(manifests) * 2), 
                                 ncol = length(manifests)))
  names(fake.data) <- manifests
  run@data <- OpenMx::mxData(fake.data, type = "raw")
  fitm <- OpenMx::mxRun(run, useOptimizer = F, silent = T)
  cov <- attr(fitm$output$algebras[[1]], "expCov")
  mean <- attr(fitm$output$algebras[[1]], "expMean")
  data <-MASS:: mvrnorm(n = N, mu = mean, Sigma = cov)
  if (N>1)   dimnames(data)[2] <- dimnames(fitm@data@observed)[2]
  return(data)
}
