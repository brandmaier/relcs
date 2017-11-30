#
# Simulation #3
#
# Generate data from RELCS model and fit with LCS model
#

# install from github if necessary
#devtools::install_github("brandmaier/relcs")

# load package
require(relcs)
require(OpenMx)

# set some random seed for reproducibility
set.seed(4519967)

simulation <- function(params) {
 
  print(params)
  N <- params[1]
  true_self_fb <- params[2]
  
  # generate data from conventional LCS
 data <- simulateDataFromRELCS(N=N, num.obs=5, autoregressionmean = true_self_fb, 
                                autoregressionvariance = 0.1,
                                residualerrorvariance = .1,
                                slopevariance = .5,
                                interceptvariance = .1)

 # fit with random effects model
 model <- createRELCS(num.obs = 5)
 fitted.model <- fit(model, data)
 summary(fitted.model)

 est.coupling <- omxGetParameters(fitted.model)
}

simulation.parameters <- expand.grid(N=c(50,100,200,500),true_self_fb=seq(-.5,.5,length.out = 7), repetitions=1:100)
#simulation.parameters <- expand.grid(N=c(50,100,200,500),true_self_fb=seq(-.5,.5,length.out = 7), repetitions=1)

# sequential execution, or...
#result = apply(X=simulation.parameters, 1, FUN=simulation)

start_time <- Sys.time()

# ...or parallel execution
cl = makeCluster(parallel::detectCores()-1)
parallel::clusterExport(cl, "simulation")
parallel::clusterEvalQ(cl, library(relcs))
parallel::clusterEvalQ(cl, library(OpenMx))
result = parApply(cl=cl, X = simulation.parameters, 1, FUN=simulation)
parallel::stopCluster(cl)

end_time <- Sys.time()

# attach results to simulation conditions
full.result <- cbind(simulation.parameters,t(result))

save(full.result,file="sim3-result.Rda")

cat("Total computation time ", end_time-start_time,"\n")

# some textual aggregates

# bias over sample size conditions
aggregate(full.result, list(full.result$N),FUN=mean)

# bias over true coupling
aggregate(full.result, list(full.result$true_self_fb),FUN=mean)


# Here, Rogier K. does his ggplot magic

# XXX
