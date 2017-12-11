#
# Simulation #2
#
# Generate data from LCS model and fit with RELCS model
#

# install from github if necessary
#devtools::install_github("brandmaier/relcs")

num.trials <- 1

# load package
require(relcs)
require(OpenMx)

# set some random seed for reproducibility
set.seed(32598)

simulation <- function(params) {

  N <- params[1]
  true_self_fb <- params[2]
  
  # generate data from conventional LCS
 model <- createLCS(num.obs = 5,autoregression = true_self_fb, has.icept = FALSE,
                    has.slope=FALSE, has.latent=FALSE)
 data <- simulateData(model, N=N)

 # fit with random effects model
 model <- createRELCS(num.obs = 5, has.icept = FALSE, has.slope = FALSE, has.latent=FALSE)
 fitted.model <- fit(model, data)
 summary(fitted.model)

 #est.coupling <- omxGetParameters(fitted.model)
 
 param.object <- summary(fitted.model)$parameters
 
 result <- c(param.object$Estimate,param.object$Std.Error)
 names(result) <- c( param.object$name, paste0("SE_",param.object$name))
 
 return(result)
}

simulation.parameters <- expand.grid(N=c(50,100,200,500),true_self_fb=seq(-.5,.5,length.out = 7), repetitions=1:num.trials)

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

save(full.result,file="sim2-result.Rda")

cat("Total computation time ", end_time-start_time,"\n")

# some textual aggregates

# bias over sample size conditions
aggregate(full.result, list(full.result$N),FUN=mean)

# bias over true coupling
aggregate(full.result, list(full.result$true_self_fb),FUN=mean)


# Here, Rogier K. does his ggplot magic

# XXX
