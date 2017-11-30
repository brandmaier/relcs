#' Create Random Effects Latent Change Score Model
#' 
#' @params num.obs Number of time points
#' @params autoregression strength of coupling parameter
#' @params residualerrorvariance residual error variance
#' @params slopevariance slope variance
#' @params has.icept Boolean. Specify whether model has a (random)intercept component
#' @params has.slope Boolean. Specify whether model has a (random)slope component
#' 
#' @export
#' 
createRELCS <- function(num.obs, ar.variance=TRUE, first.obs.var.unique=FALSE, 
                                slope.ar.cov=FALSE, has.slope=TRUE, has.icept=TRUE) {
  
  manifests<- paste0("X",1:num.obs)
  
  randomar <- "coup"
  randomslp <- "slope"
  randomicept <- "icept"
  deltas <- paste0("delta",1:(num.obs-1))
  etas <- paste0("eta",1:num.obs)
  arlabels <- paste0("data.X",1:length(deltas))
  
  latents<- c(etas, deltas, randomar)
  if (has.slope) latents <- c(latents, randomslp)
  if (has.icept) latents <- c(latents, randomicept)
  # from eta to manifest
  p1 <- mxPath(from=etas,to=manifests,free = FALSE,value=1, arrows=1)
  
  # residual variance
  p2 <- mxPath(from=manifests,to=manifests,free=TRUE,
               labels=pkg.globals$RESIDUAL_ERROR,value=0.1,arrows = 2)
  
  #from delta to eta
  p3 <- mxPath(from=deltas,etas[2:length(etas)],free=FALSE,value=1,arrows=1)
  
  # fixed AR
  p4 <- mxPath(from=etas[1:(length(etas)-1)], 
               to=etas[2:length(etas)], arrows=1,value=1, free=FALSE)
  
  # random to delta   ( definition variable paths)
  p5 <- mxPath(from=randomar, to=deltas, arrow=1, labels=arlabels, free=FALSE)
  
  # conventional ar_path
  # p5 <- mxPath(from=etas[1:(length(etas)-1)], to=deltas[1:length(deltas)], arrows=1,value=0, free=TRUE,
  #               label="ar_mu")
  
  # ar variance
  if (ar.variance) {
    p6 <- mxPath(from=randomar,to=randomar, arrow=2, 
                 labels=pkg.globals$SELF_FEEDBACK_RE, free=TRUE, value=1)
  } else {
    p6 <- mxPath(from=randomar,to=randomar, arrow=2,
                 labels=pkg.globals$SELF_FEEDBACK_RE, free=FALSE, value=0)
    
  }
  
  # mean to first
  p7 <- mxPath(from="one", to=etas[1],free=TRUE,arrows=1,label=pkg.globals$INTERCEPT_FE)
  
  # mean to ar
  p8 <- mxPath(from="one", to=randomar,arrow=1,
               labels=pkg.globals$SELF_FEEDBACK_FE,free=TRUE, values=0)
  #  p8 <- mxPath(from="one", to=randomar,arrow=1,free=FALSE, values=0)
  
  
  # random slope to deltas
  p9 <- mxPath(from=randomslp, to=deltas, arrow=1, free=FALSE, value=1)
  
  # mu to random slope
  p10 <- mxPath(from="one",to=randomslp, arrow=1,free=TRUE, labels=pkg.globals$SLOPE_FE, value=0)
  
  # random slope variance
  p11 <- mxPath(from=randomslp, to=randomslp,arrows=2,free=TRUE,labels=pkg.globals$SLOPE_RE, value=1)
  
  if (!has.slope) {
    p9 <- NULL
    p10 <- NULL
    p11 <- NULL
  }
  
  # first unique variance
  if (first.obs.var.unique) {
    idx <- c(1)
    p12 <- mxPath(from=manifests[idx],
                  to=manifests[idx],free=TRUE,arrows=2,
                  value=.1,labels=paste0("initial_variance",idx))
    #    p12 <- mxPath(from=manifests[1],to=manifests[1],free=TRUE,arrows=2,value=.1,labels="initial_variance")
  } else {
    p12 <- NULL
  }
  
  # slope-ar-correlation
  if (slope.ar.cov) {
    p13 <- mxPath(from=randomar, to=randomslp, arrows=2, 
                  free=TRUE,value=0.1,label="ar_slope_cov")
  } else {
    p13 <- NULL
  }
  
  # random icept variance
  if (has.icept) {
    p14 <- mxPath(from=randomicept, to=etas[1],arrows=1, free=FALSE) # TODO?
    p15 <- mxPath(from=randomicept, to=randomicept, arrows=2, free=TRUE, value=1, labels=pkg.globals$INTERCEPT_RE)
  } else {
    p14 <- NULL
    p15 <- NULL
  }
  
  model <- mxModel("AugmentedLCSmodel", 
                   type="RAM",
                   manifestVars = manifests,
                   latentVars = latents,
                   p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15
  );
  
  return(model)
  
}