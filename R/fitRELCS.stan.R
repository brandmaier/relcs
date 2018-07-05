#'
#' @export
create_default_priors <- function() {
  return(list(intt_var="gamma(.1,2)", sfb_var="gamma(.1,2)",sfb_mean="normal(0,1)"))
}

#'
#' @export
fitRELCS.stan <- function(num.obs, data, cores=3, chains=3, iter=500, algorithm=algorithm,
                          seed=sample.int(.Machine$integer.max, 1), warmup=warmup, control=NULL,
                          priors=create_default_priors())
{
  
  
  
relcs.stan <-paste0("
data{
int N; // sample size
int t; 
vector[t] X[N]; // data matrix of order [N,P]
}

parameters{
real ",pkg.globals$RESIDUAL_ERROR,"; // error variance for each observation
real<lower=0> ",pkg.globals$SELF_FEEDBACK_RE,"; // self-feedback variance (positive)
real ",pkg.globals$SELF_FEEDBACK_FE,"; // self-feedback mean
vector[N] beta; // self-feedbacks for each person

real<lower=0> ",pkg.globals$INTERCEPT_RE,";
real intt_mean; // intercept mean
vector[N] intt;
}

transformed parameters{
vector[t] mu[N];
vector[t-1] d[N];

for (i in 1:N){

mu[i,1] = intt[i];

for (tt in 2:t){
d[i,tt-1] = beta[i]*mu[i,tt-1];
mu[i,tt] = d[i,tt-1]+mu[i,tt-1];
}
}
}

model{
",pkg.globals$INTERCEPT_RE," ~ ",priors$intt_var,";  // prior for intercept variance\n",
pkg.globals$SELF_FEEDBACK_RE," ~ ",priors$sfb_var,";   // prior for self-feedback variance\n",
pkg.globals$SELF_FEEDBACK_FE," ~ ",priors$sfb_mean,";  // prior for self-feedback mean

for (i in 1:N){

 intt[i] ~ normal(intt_mean, pow(",pkg.globals$INTERCEPT_RE,",0.5));
 beta[i] ~ normal(",pkg.globals$SELF_FEEDBACK_FE,", pow(",pkg.globals$SELF_FEEDBACK_RE,",0.5));
 X[i,1] ~ normal(mu[i,1],pow(",pkg.globals$RESIDUAL_ERROR,",0.5));


 for (tt in 2:t){
   X[i,tt] ~ normal(mu[i,tt], pow(",pkg.globals$RESIDUAL_ERROR,",0.5));
 }

}
}
")

stan.data <- list(N=nrow(data),X=data,t=ncol(data))

lcs.out=stan(model_code=relcs.stan,iter=iter,seed=seed, warmup = warmup, algorithm=algorithm,
             data = stan.data,chains=chains,cores=cores,control=control,
             pars=c(pkg.globals$RESIDUAL_ERROR,pkg.globals$SELF_FEEDBACK_FE,pkg.globals$SELF_FEEDBACK_RE,"intt_mean",pkg.globals$INTERCEPT_RE))

result <- list()
result$fit <- lcs.out
result$language <- "stan"
class(result) <- "relcs.fitted"

return(result)

}
