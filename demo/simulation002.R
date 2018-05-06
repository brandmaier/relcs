#
# Simulation #2
#
# Generate data from LCS model and fit with RELCS model
#
# Expectation: RELCS model has a superfluous component (variance of self feedback),
#   which should be estimated close to zero as it does not exists in the data generator (regular LCS)
#
#

# install from github if necessary
#devtools::install_github("brandmaier/relcs")
mypath = "./" #/home/mpib/brandmaier/"

parallel <- TRUE

# a) number of MC trials per condition
num.trials <- 1000

# b)
simulation.parameters <- expand.grid(
  N=c(50,100,200,500),
  #N=200,
  true_self_fb=seq(-.5,.5,length.out = 7), 
  #true_self_fb=.1,
  repetitions=1:num.trials)

# c) other model-specification-related parameters

has.latent <- TRUE


# load package
require(relcs)
require(OpenMx)

# set some random seed for reproducibility
set.seed(32598)

simulation <- function(params) {

  require(relcs)
  require(OpenMx)
  
  N <- params[1]
  true_self_fb <- params[2]
  
  # generate data from conventional LCS
 model <- createLCS(num.obs = 5,autoregression = true_self_fb, has.icept = TRUE,
                    has.slope=TRUE)
 data <- simulateData(model, N=N)

 # fit with random effects model
 model <- createRELCS(num.obs = 5, has.icept = TRUE, has.slope = TRUE)
 fitted.model <- fit(model, data)
 ref.relcs <- mxRefModels(fitted.model,run=TRUE)
 sm.relcs <- summary(fitted.model, refModels=ref.relcs)


 #est.coupling <- omxGetParameters(fitted.model)
 
 param.object <- summary(fitted.model)$parameters
 
 result <- c(param.object$Estimate,param.object$Std.Error)
 names(result) <- c( param.object$name, paste0("SE_",param.object$name))
 result <- c(result, Chi=sm.relcs$Chi, RMSEA=sm.relcs$RMSEA, CFI=sm.relcs$CFI, status=fitted.model$output$status$code)
 
 return(result)
}


# sequential execution, or...
#result = apply(X=simulation.parameters, 1, FUN=simulation)

start_time <- Sys.time()

cores <- Sys.getenv("PBS_NUM_PPN")
cores <- if (cores == '') parallel::detectCores()-1 else as.numeric(cores)


cat("Using ",cores, " CPUS\n")

if (parallel) {
  # ...or parallel execution
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

save(full.result,file=paste0(mypath,"sim2-result.Rda"))

cat("Total computation time ", end_time-start_time,"\n")

# some textual aggregates

# bias over sample size conditions
aggregate(full.result, list(full.result$N),FUN=mean)

# bias over true coupling
aggregate(full.result, list(full.result$true_self_fb),FUN=mean)


# Here, Rogier K. does his ggplot magic

# XXX
