#
# create a LCS model
# and use onyxR to visualize it
#

devtools::install_local("~/Documents/Projects/relcs")
require(onyxR)
require(relcs)
require(OpenMx)

num.obs <- 8

# simulate data from RELCS
data <- relcs::simulateDataFromRELCS(100,num.obs,autoregressionmean = .4, autoregressionvariance = 0, 
                                     residualerrorvariance = .1,
                             slopevariance = 1, interceptvariance = 1,
                             has.slope=TRUE, has.icept=TRUE)

# fit using non-RELCS
model <- createLCS(num.obs = num.obs)
model@data <- mxData(data,"raw")

run <- mxRun(model)

summary(run)
