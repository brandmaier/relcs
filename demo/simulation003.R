#
# Simulation #3
#
# Generate data from RELCS model and fit with LCS model
#
# Expectation: The true process has variation in the self-feedback but we are using
#   a fixed-effects-only model. Average feedback should be estimated okayish but
#   probably the true variation decreases precision of estimates, particularly, 
#   residual error (?)
#
mypath = "./" #"/home/mpib/brandmaier/"

parallel <- TRUE

num.trials <- 1000
#num.trials <- 20

# b)
simulation.parameters <- expand.grid(
          N=c(50,100,200,500),
          #N=200,
          true_self_fb=seq(-.5,.5,length.out = 7), 
          #  true_self_fb=.1,
          repetitions=1:num.trials
          )

# c) other model-specification-related parameters

# ...

# install from github if necessary
#devtools::install_github("brandmaier/relcs")

# load package
require(relcs)
require(OpenMx)

# set some random seed for reproducibility
set.seed(4519967)

simulation <- function(params) {
 
  require(relcs)
  require(OpenMx)
  
  
  print(params)
  N <- params[1]
  true_self_fb <- params[2]
  num.obs <- 5
  
  # generate data from conventional LCS
 data <- simulateDataFromRELCS(N=N, num.obs=num.obs, autoregressionmean = true_self_fb, 
                                autoregressionvariance = 0.1,
                                residualerrorvariance = .1,
                                slopevariance = .5,
                                interceptvariance = .1,
                               interceptmu = 0,
                               slopemu = 1,
                                has.icept = TRUE,
                               has.slope = TRUE)

 # fit with random effects model
 model <- createRELCS(num.obs = num.obs, has.icept = TRUE,
                      has.slope = TRUE)
 fitted.model.relcs <- fit(model, data)
 ref.relcs <- mxRefModels(fitted.model.relcs,run=TRUE)
 sm.relcs <- summary(fitted.model.relcs, refModels=ref.relcs)
 
 # fit with fixed effects model
 model <- createLCS(num.obs = num.obs,has.icept = TRUE, has.slope = TRUE)
 fitted.model.lcs <- fit(model, data)
 ref.lcs <- mxRefModels(fitted.model.lcs,run=TRUE)
 sm.lcs <- summary(fitted.model.lcs, refModels=ref.lcs)

 est.coupling <- c(omxGetParameters(fitted.model.relcs),
                   Chi=sm.relcs$Chi, RMSEA=sm.relcs$RMSEA, CFI=sm.relcs$CFI, status=fitted.model.relcs$output$status$code,
                   omxGetParameters(fitted.model.lcs),
                   Chi=sm.lcs$Chi, RMSEA=sm.lcs$RMSEA, sCFI=sm.lcs$CFI, status=fitted.model.lcs$output$status$code
                   )
 names(est.coupling)[1:11] <- paste0("relcs_",names(est.coupling)[1:11])
 names(est.coupling)[12:22] <- paste0("lcs_",names(est.coupling)[12:22])
 return(est.coupling)
}

#simulation.parameters <- expand.grid(N=c(50,100,200,500),true_self_fb=seq(-.5,.5,length.out = 7), repetitions=1)

# sequential execution, or...
#result = apply(X=simulation.parameters, 1, FUN=simulation)

start_time <- Sys.time()

cores <- Sys.getenv("PBS_NUM_PPN")
cores <- if (cores == '') parallel::detectCores()-1 else as.numeric(cores)

cat("Using ",cores, " CPUS\n")

# ...or parallel execution
if (parallel) {
  cl = makeCluster(cores)
  parallel::clusterExport(cl, c("simulation"))
#  parallel::clusterEvalQ(cl, library(relcs))
#  parallel::clusterEvalQ(cl, library(OpenMx))
  result = parApply(cl=cl, X = simulation.parameters, 1, FUN=simulation)
  parallel::stopCluster(cl)
} else {
  result = apply(X = simulation.parameters, 1, FUN=simulation)
}
end_time <- Sys.time()

# attach results to simulation conditions
full.result <- cbind(simulation.parameters,t(result))

save(full.result,file=paste0(mypath,"sim3-result.Rda"))

cat("Total computation time ", end_time-start_time,"\n")

# some textual aggregates

# bias over sample size conditions
aggregate(full.result, list(full.result$N),FUN=mean)

# bias over true coupling
aggregate(full.result, list(full.result$true_self_fb),FUN=mean)


# Here, Rogier K. does his ggplot magic

# XXX
