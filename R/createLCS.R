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
                       slopevariance=0.1, interceptvariance=0.1, interceptmu=0,
                      slopemu = 0,
                      has.slope=TRUE, has.icept=TRUE, has.latent=TRUE) {
  
  manifests<- paste0("X",1:num.obs)
  
  # randomar <- "coup"
  randomslp <- "slope"
  randomicept <- "icep"
  deltas <- paste0("delta",1:(num.obs-1))
  
  if (has.latent)
    etas <- paste0("eta",1:num.obs)
  else
    etas <- NULL
  
  arlabels <- paste0("data.X",1:length(deltas))
  
  latents<- c(etas, deltas )
  if (has.slope) latents <- c(latents, randomslp)
  if (has.icept) latents <- c(latents, randomicept)

    # from eta to manifest
  if (has.latent)
    p1 <- mxPath(from=etas,to=manifests,free = FALSE,value=1, arrows=1)
  else
    p1 <- NULL
  
  # residual variance
  p2 <- mxPath(from=manifests,to=manifests,free=TRUE,labels=pkg.globals$RESIDUAL_ERROR,
               value=residualerrorvariance,arrows = 2)
  
  #from delta to eta
  if (has.latent)
    p3 <- mxPath(from=deltas,etas[2:length(etas)],free=FALSE,value=1,arrows=1)
  else 
    p3 <- mxPath(from=deltas,manifests[2:length(manifests)],free=FALSE,value=1,arrows=1)    
  
  # fixed feedback over time
  if (has.latent)
    p4 <- mxPath(from=etas[1:(length(etas)-1)], to=etas[2:length(etas)], 
               arrows=1,value=1, free=FALSE)
  else
    p4 <- mxPath(from=manifests[1:(length(manifests)-1)], to=manifests[2:length(manifests)], 
                 arrows=1,value=1, free=FALSE)  
  # random to delta   ( definition variable paths)
  # p5 <- mxPath(from=randomar, to=deltas, arrow=1, labels=arlabels, free=FALSE)
  
  # ar variance
  #  p6 <- mxPath(from=randomar,to=randomar, arrow=2, labels="ar_variance", free=TRUE, value=1)
  
  # self-feedback fixed effect
  if (has.latent)
  p5 <- mxPath(from=etas[1:(length(etas)-1)], to=deltas[1:length(deltas)],
               arrows=1,value=autoregression, free=TRUE,
               label=pkg.globals$SELF_FEEDBACK_FE)
  else
    p5 <- mxPath(from=manifests[1:(length(manifests)-1)], to=deltas[1:length(deltas)],
                 arrows=1,value=autoregression, free=TRUE,
                 label=pkg.globals$SELF_FEEDBACK_FE)    
  
  # mean to first
  #p7 <- mxPath(from="one", to=etas[1],free=TRUE,arrows=1,label="constT1",values=0)
  p7 <- NULL
  
  # mean to ar
  # p8 <- mxPath(from="one", to=randomar,arrow=1,labels="ar_mu",free=TRUE, values=0)
  
  # random slope to deltas
  p9 <- mxPath(from=randomslp, to=deltas, arrow=1, free=FALSE, value=1)
  
  # mu to random slope
  if (has.slope)
    p10 <- mxPath(from="one",to=randomslp, arrow=1,free=TRUE, 
                labels=pkg.globals$SLOPE_FE, value=slopemu)
  else 
    p10 <- mxPath(from="one",to=deltas, arrow=1,free=TRUE, 
                  labels=pkg.globals$SLOPE_FE, value=slopemu)
  
  # random slope variance
  p11 <- mxPath(from=randomslp, to=randomslp,arrows=2,free=TRUE,
                labels=pkg.globals$SLOPE_RE, value=slopevariance)
  
  if (!has.slope) {
    p9 <- mxPath(from=deltas,to=deltas,arrows=2,free=TRUE,labels=pkg.globals$SLOPE_FE,
                 connect = "single")
    p11 <- NULL
  }
  
  # random icept variance
  if (has.icept) {
    p12 <- mxPath(from=randomicept, to=etas[1],arrows=1, free=FALSE,values=1)
    p13 <- mxPath(from=randomicept, to=randomicept, arrows=2, free=TRUE,
                  values=interceptvariance, labels=pkg.globals$INTERCEPT_RE)
    p14 <- mxPath(from="one", to=randomicept,arrows=1,free=TRUE,values=interceptmu, labels=pkg.globals$INTERCEPT_FE)
  } else {
    p12 <- NULL
    p13 <- NULL
    p14 <- NULL
  }
  
  if (has.icept && has.slope) {
    p7 <- mxPath(from=randomicept, to=randomslp, arrows=2, free=TRUE,value=0, labels=pkg.globals$INTERCEPT_SLOPE_COV)
  } else {
    p7 <- NULL
  }
  
  model <- mxModel("LCSmodel", 
                   type="RAM",
                   manifestVars = manifests,
                   latentVars = latents,
                   p1,p2,p3,p4,p5,p7,p9,p10,p11,p12,p13,p14
  );
  
  return(model)
  
}
