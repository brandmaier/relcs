#
# Simulation #1
#
# Generate data from RELCS model and fit with RELCS model. 
#
# This can be run locally (parallel execution supported) or be run on
# a PBS grid with multiple cores.
#
# Here are a couple of simulation parameters that can be easily adjusted:

parallel <- TRUE

# a) number of MC trials per condition
num.trials <- 1000

# b)
simulation.parameters <- expand.grid(
  # sample size conditions
  #N=c(50,100,200,500),
  N=200,
  # Self-feedback range to test
#  true_self_fb=seq(-.5,.5,length.out = 7),
  true_self_fb=.1,
  # Self-feedback variance
  true_self_fb_var=c(0,0.001,0.01,0.1),
  # Number of observations
  num_obs = c(3,5,7),
  # number of repeated trials, see (a)
  repetitions=1:num.trials)

# c) other model-specification-related parameters



#
# ---------- DO NOT CHANGE BELOW --------------
#

# install from github if necessary
#devtools::install_github("brandmaier/relcs")

# load package
require(relcs)
require(OpenMx)

# set some random seed for reproducibility
set.seed(228825)

simulation <- function(params) {
  
  #
  print(params)
  N <- params[1]
  true_self_fb <- params[2]
  true_self_fb_var <- params[3]
  num.obs <- params[4]
  
  # generate data from conventional LCS
  data <- simulateDataFromRELCS(N=N, num.obs=num.obs, autoregressionmean = true_self_fb, 
                                autoregressionvariance = true_self_fb_var,
                                residualerrorvariance = .1,
                                slopevariance = .5, 
                                interceptvariance = .1,
                                has.icept =  TRUE,
                                has.slope = TRUE)
  
  # fit with random effects model
  model <- createRELCS(num.obs = num.obs, has.icept = TRUE, has.slope = TRUE)
  fitted.model <- fit(model, data)
  summary(fitted.model)
  
  param.object <- summary(fitted.model)$parameters
  
  result <- c(param.object$Estimate,param.object$Std.Error)
  names(result) <- c( param.object$name, paste0("SE_",param.object$name))
  
  return(result)
  #est.coupling <- omxGetParameters(fitted.model)
}

# 
# Fire up cluster
#

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
  parallel::clusterEvalQ(cl, library(relcs))
  parallel::clusterEvalQ(cl, library(OpenMx))
  result = parApply(cl=cl, X = simulation.parameters, 1, FUN=simulation)
  parallel::stopCluster(cl)
} else {
  result = apply(X = simulation.parameters, 1, FUN=simulation)
}

end_time <- Sys.time()

cat("Total computation time ", end_time-start_time,"\n")

# Save results
# -----
# attach results to simulation conditions
#

full.result <- cbind(simulation.parameters,t(result))

save(full.result,file="sim1-result.Rda")




#
# Output
#

# preliminary textual aggregates

# bias over sample size conditions
aggregate(full.result, list(full.result$N),FUN=mean)

# bias over true coupling
aggregate(full.result, list(full.result$true_self_fb),FUN=mean)


# Here, Rogier K. does his ggplot magic

# XXX
