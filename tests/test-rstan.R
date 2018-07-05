require("relcs")

num.obs <- 5
N <- 500

# create data
sdata <- relcs::simulateDataFromRELCS(N=500, num.obs = num.obs,autoregressionmean = .2,
                             autoregressionvariance = .05,residualerrorvariance = 1,
                             slopemu = 0, slopevariance = 0, interceptmu = 5, interceptvariance = 1,
                             has.slope=FALSE)


# use STAN
control <- list(adapt_delta=0.99,max_treedepth=20)
control <- NULL
iter <- 200
algorithm <- "NUTS"
#algorithm <- "HMC"
result <- fitRELCS.stan(num.obs = num.obs, data=sdata, iter=iter,
                        warmup=iter/5, control=control, algorithm=algorithm)


print( getEstimates(result) )
