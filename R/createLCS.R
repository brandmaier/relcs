#' Create Latent Change Score Model
#' 
#' @params num.obs Number of time points
#' @params autoregression strength of coupling parameter
#' @params residualerrorvariance residual error variance
#' @params slopevariance slope variance
#' @params has.icept Boolean. Specify whether model has a (random)intercept component
#' @params has.slope Boolean. Specify whether model has a (random)slope component
#' 
#' @export
createLCS <- function(num.obs, autoregression=.3, residualerrorvariance=.1, 
                       slopevariance=0.1, interceptvariance=0.1,has.slope=TRUE, has.icept=TRUE) {
  
  manifests<- paste0("X",1:num.obs)
  
  # randomar <- "coup"
  randomslp <- "slope"
  randomicept <- "icep"
  deltas <- paste0("delta",1:(num.obs-1))
  etas <- paste0("eta",1:num.obs)
  arlabels <- paste0("data.X",1:length(deltas))
  
  latents<- c(etas, deltas )
  if (has.slope) latents <- c(latents, randomslp)
  if (has.icept) latents <- c(latents, randomicept)
  # from eta to manifest
  p1 <- mxPath(from=etas,to=manifests,free = FALSE,value=1, arrows=1)
  
  # residual variance
  p2 <- mxPath(from=manifests,to=manifests,free=TRUE,labels="sigma_err",value=residualerrorvariance,arrows = 2)
  
  #from delta to eta
  p3 <- mxPath(from=deltas,etas[2:length(etas)],free=FALSE,value=1,arrows=1)
  
  # fixed AR
  p4 <- mxPath(from=etas[1:(length(etas)-1)], to=etas[2:length(etas)], arrows=1,value=1, free=FALSE)
  
  # random to delta   ( definition variable paths)
  # p5 <- mxPath(from=randomar, to=deltas, arrow=1, labels=arlabels, free=FALSE)
  
  # ar variance
  #  p6 <- mxPath(from=randomar,to=randomar, arrow=2, labels="ar_variance", free=TRUE, value=1)
  
  
  p5 <- mxPath(from=etas[1:(length(etas)-1)], to=deltas[1:length(deltas)], arrows=1,value=autoregression, free=TRUE,
               label="ar_mu")
  
  
  # mean to first
  p7 <- mxPath(from="one", to=etas[1],free=TRUE,arrows=1,label="constT1")
  
  # mean to ar
  # p8 <- mxPath(from="one", to=randomar,arrow=1,labels="ar_mu",free=TRUE, values=0)
  
  # random slope to deltas
  p9 <- mxPath(from=randomslp, to=deltas, arrow=1, free=FALSE, value=1)
  
  # mu to random slope
  p10 <- mxPath(from="one",to=randomslp, arrow=1,free=TRUE, labels="slope_mu", value=0)
  
  # random slope variance
  p11 <- mxPath(from=randomslp, to=randomslp,arrows=2,free=TRUE,labels="slope_variance", value=slopevariance)
  
  if (!has.slope) {
    p9 <- NULL
    p10 <- NULL
    p11 <- NULL
  }
  
  # random icept variance
  if (has.icept) {
    p12 <- mxPath(from=randomicept, to=etas[1],arrows=1, free=FALSE)
    p13 <- mxPath(from=randomicept, to=randomicept, arrows=2, free=TRUE, value=interceptvariance, labels="icept_variance")
  } else {
    p12 <- NULL
    p13 <- NULL
  }
  
  model <- mxModel("LCSmodel", 
                   type="RAM",
                   manifestVars = manifests,
                   latentVars = latents,
                   p1,p2,p3,p4,p5,p7,p9,p10,p11,p12,p13
  );
  
  return(model)
  
}
