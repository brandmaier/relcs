fitRELCS.stan <- function(num.obs, data, cores=3, chains=3, iter=500, algorithm=algorithm,
                          seed=sample.int(.Machine$integer.max, 1), warmup=warmup, control=NULL)
{
  
  
relcs.stan <-"
data{
int N; // sample size
int t; 
vector[t] X[N]; // data matrix of order [N,P]
}

parameters{
real sigma; // error variance for each observation
real<lower=0> sfb_var; // self-feedback variance (positive)
real sfb_mean; // self-feedback mean
vector[N] beta; // self-feedbacks for each person

real<lower=0> intt_var;
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
intt_var ~ gamma(.1,2);  // prior for intercept variance
sfb_var ~ gamma(.1,2);   // prior for self-feedback variance
sfb_mean ~ normal(0,1);  // prior for self-feedback mean

for (i in 1:N){

 intt[i] ~ normal(intt_mean, pow(intt_var,0.5));
 beta[i] ~ normal(sfb_mean, sfb_var);
 X[i,1] ~ normal(mu[i,1],pow(sigma,0.5));


 for (tt in 2:t){
   X[i,tt] ~ normal(mu[i,tt], pow(sigma,0.5));
 }

}
}
"

stan.data <- list(N=nrow(data),X=data,t=ncol(data))

lcs.out=stan(model_code=relcs.stan,iter=iter,seed=seed, warmup = warmup, algorithm=algorithm,
             data = stan.data,chains=chains,cores=cores,control=control,
             pars=c("sigma","sfb_mean","sfb_var","intt_mean","intt_var"))

return(lcs.out)

}
