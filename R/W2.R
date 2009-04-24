`W2` <-
function(data){
      
      if (is.numeric(data)){
           x<-sort(data)
           n<-length(x)
           estimators<-sn.mle(y=x,plot.it=FALSE)$cp
           estimators[1]=mean(x)
           estimators[2]=sd(x)
           direct.estimators=cp.to.dp(estimators)
           total=0
           for (i in 1:n){
                zi=psn(x[i],loc=direct.estimators[1],scale=direct.estimators[2],shape=direct.estimators[3])
                total=total + (zi - (2*i-1)/(2*n))^2
           }
           w2= total+ 1/(12*n)
           return(w2)}
      else stop("argument 'data' must be numeric")
}

