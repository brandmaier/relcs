#
# Simulation #1
#
# Generate data from LCS model and fit with RELCS model
#

# install from github if necessary
#devtools::install_github("brandmaier/relcs")

# load package
require(relcs)
require(OpenMx)

simulation <- function(params) {

  N <- params[1]
  true.coupling <- params[2]
  
  # generate data from conventional LCS
 model <- createLCS(num.obs = 5,autoregression = true.coupling)
 data <- simulateData(model, N=N)

 # fit with random effects model
 model <- createRELCS(num.obs = 5)
 fitted.model <- fit(model, data)
 summary(fitted.model)

 est.coupling <- omxGetParameters(fitted.model)["ar_mu"]
}

simulation.parameters <- expand.grid(N=c(50,100,200,500),true.coupling=c(0,.1,.2), repetitions=1:10)

# sequential execution, or...
#result = apply(X=simulation.parameters, 1, FUN=simulation)

# ...or parallel execution
cl = makeCluster(parallel::detectCores()-1)
parallel::clusterExport(cl, "simulation")
parallel::clusterEvalQ(cl, library(relcs))
parallel::clusterEvalQ(cl, library(OpenMx))
result = parApply(cl=cl, X = simulation.parameters, 1, FUN=simulation)
parallel::stopCluster(cl)

# attach results to simulation conditions
full.result <- cbind(simulation.parameters,result)


# Here, Rogier K. does his ggplot magic

# XXX
