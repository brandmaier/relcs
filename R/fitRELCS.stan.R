#'
#' @export
create_default_priors <- function() {
  return(list(intt_var="gamma(.1,2)", sfb_var="gamma(.1,2)",sfb_mean="normal(0,1)"))
}

#'
#' @export
fitRELCS.stan <- function(num.obs, data, cores=3, chains=3, iter=600, algorithm="NUTS",
                          seed=sample.int(.Machine$integer.max, 1), warmup=200, control=NULL,
                          priors=create_default_priors(), no.run=FALSE, has.slope=FALSE, 
                          pred.sfb.data=NULL,...)
{
  
pred.sfb <- ""
pred.sfb.vars <- ""
if (!is.null(pred.sfb.data)) {
  # TODO: create predictor string
  param.nms <- paste0("sfb_beta",1:ncol(pred.sfb.data))
  data.nms <- paste0("sfb_pred",1:ncol(pred.sfb.data))
  paste(param.nms,data.nms, collapse = "+")
}

  
#if (is.null(control)) {
#  control=list(adapt_delta=0.99,max_treedepth=20)
#}
  
missing.part1 <- "int<lower=0> Ncomp; // Number of non-missing values
int<lower=0> Nmiss; // Number of missing values
real dat_complete[Ncomp];   // Vector of non-missing values
int ind_pres[Ncomp, 2];     // Matrix (row, col) of non-missing value indices
int ind_miss[Nmiss, 2];     // Matrix (row, col) of missing value indices
"  

nonmissing.part1 <- "vector[t] X[N]; // data matrix of order [N,P]"

missing.part2 <- "// Vector containing stochastic nodes (for filling missing values
real Xmiss[Nmiss]; "
nonmissing.part2 <- ""

missing.part3 <- "vector[t] X[N];   // The data with interpolated missing values"
nonmissing.part3 <- ""

missing.part4 <- "for(n in 1:Ncomp) {
X[ind_pres[n,1]][ind_pres[n,2]] = dat_complete[n];
}

for(n in 1:Nmiss){
X[ind_miss[n,1]][ind_miss[n,2]] = Xmiss[n];
}"

nonmissing.part4 <- ""

has.missing <- any(is.na(data))

if (has.missing) {
  print("Running missing data model.")
}
  
relcs.stan <-paste0("
data{
int N; // sample size
int t; 
",ifelse(has.missing,missing.part1,nonmissing.part1),"
}

parameters{
real ",pkg.globals$RESIDUAL_ERROR,"; // error variance for each observation
real<lower=0> ",pkg.globals$SELF_FEEDBACK_RE,"; // self-feedback variance (positive)
real ",pkg.globals$SELF_FEEDBACK_FE,"; // self-feedback mean
vector[N] beta; // self-feedbacks for each person

real<lower=0> ",pkg.globals$INTERCEPT_RE,";
real ",pkg.globals$INTERCEPT_FE,"; // intercept mean
vector[N] intt;

",ifelse(has.missing,missing.part2,nonmissing.part2),"
}

transformed parameters{

",ifelse(has.missing,missing.part3,nonmissing.part3),"

vector[t] mu[N];
vector[t-1] d[N];

for (i in 1:N){

mu[i,1] = intt[i];

for (tt in 2:t){
d[i,tt-1] = beta[i]*mu[i,tt-1];
mu[i,tt] = d[i,tt-1]+mu[i,tt-1];
}
}

",ifelse(has.missing,missing.part4,nonmissing.part4),"

}

model{
",pkg.globals$INTERCEPT_RE," ~ ",priors$intt_var,";  // prior for intercept variance\n",
pkg.globals$SELF_FEEDBACK_RE," ~ ",priors$sfb_var,";   // prior for self-feedback variance\n",
pkg.globals$SELF_FEEDBACK_FE," ~ ",priors$sfb_mean,";  // prior for self-feedback mean

for (i in 1:N){

 intt[i] ~ normal(",pkg.globals$INTERCEPT_FE,", pow(",pkg.globals$INTERCEPT_RE,",0.5));
 beta[i] ~ normal(",pkg.globals$SELF_FEEDBACK_FE,pred_sfb,", pow(",pkg.globals$SELF_FEEDBACK_RE,",0.5));
 X[i,1] ~ normal(mu[i,1],pow(",pkg.globals$RESIDUAL_ERROR,",0.5));


 for (tt in 2:t){
   X[i,tt] ~ normal(mu[i,tt], pow(",pkg.globals$RESIDUAL_ERROR,",0.5));
 }

}
}
")

if (!has.missing) {

stan.data <- list(N=nrow(data),X=data,t=ncol(data))

} else {

  dat_complete <- data[!is.na(data)]

  ind_pres <- which(!is.na(data), arr.ind = TRUE)
  ind_miss <- which(is.na(data), arr.ind = TRUE)


stan.data <- list(N = nrow(data),
                 t = ncol(data),
                 Ncomp = length(dat_complete),
                 Nmiss = sum(is.na(data)),
                 dat_complete = dat_complete,
                 ind_pres = ind_pres,
                 ind_miss = ind_miss)

}

if (no.run) {
 lcs.out <- NULL 
} else {
  lcs.out=stan(model_code=relcs.stan,iter=iter,seed=seed, warmup = warmup, algorithm=algorithm,
             data = stan.data,chains=chains,cores=cores,control=control,
             pars=c(pkg.globals$RESIDUAL_ERROR,pkg.globals$SELF_FEEDBACK_FE,pkg.globals$SELF_FEEDBACK_RE,pkg.globals$INTERCEPT_FE,pkg.globals$INTERCEPT_RE))
}

result <- list()
result$fit <- lcs.out
result$language <- "stan"
result$code <- relcs.stan
result$stan.data <- stan.data
class(result) <- "relcs.fitted"

return(result)

}


