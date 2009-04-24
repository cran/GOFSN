`D` <-
function(data){
     
      if (is.numeric(data)){
           x<-sort(data)
           n<-length(x)
           estimators<-sn.mle(y=x,plot.it=FALSE)$cp
           estimators[1]=mean(x)
           estimators[2]=sd(x)
           direct.estimators=cp.to.dp(estimators)
           greater.vector=numeric(length(x))
           less.vector=numeric(length(x))
           for (i in 1:n){
                zi=psn(x[i],loc=direct.estimators[1],scale=direct.estimators[2],shape=direct.estimators[3])
                greater.vector[i]=(i/n - zi)
                less.vector[i]=(zi - (i-1)/n )
           }
           d=max(c(greater.vector,less.vector))
           return(sqrt(n)*d)}
      else stop("argument 'data' must be numeric")
}

