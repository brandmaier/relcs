#' @S3method plot relcs.estimates
plot.relcs.estimates <- function(x) {
 est<-x$estimates
  
  est.df <- data.frame(value=est,name=names(est))
  ggplot(data=est.df,aes(x=name,y=value))+
    geom_bar(stat="identity",fill="steelblue")+coord_flip()+
    theme_minimal()
  
}


#' @S3method print relcs.estimates
print.relcs.estimates <- function(x) {
  
  if (!is.null(x))
    print(x$estimate)
}  