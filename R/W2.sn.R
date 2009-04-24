`W2.sn` <-
function(data){
      data(W2.quantiles)
      if (is.numeric(data)){
          W2obs=W2(data)
          size=length(data)
          if (size<5) stop("not enough 'data', size of 'data' must be at least 5")
          if (size>=5 && size<=50) row=size-4 
          else if (size>=50 && size<= 54) row=46
          else if (size>=55 && size<= 64) row=47
          else if (size>=65 && size<= 74) row=48
          else if (size>=75 && size<= 84) row=49
          else if (size>=85 && size<= 94) row=51
          else row=51 
          print(paste("Observed W2 for data set with sample size n=",size,":",W2obs))
          if (W2obs<W2.quantiles[row,1]) print("p-value > 0.10")
          else if (W2.quantiles[row,1]<W2obs && W2obs<W2.quantiles[row,2]) print("0.05 < p-value < 0.10")
          else if (W2.quantiles[row,2]<W2obs && W2obs<W2.quantiles[row,3]) print("0.025 < p-value < 0.05")
          else if (W2.quantiles[row,3]<W2obs && W2obs<W2.quantiles[row,4]) print("0.010 < p-value < 0.025")
          else if (W2.quantiles[row,4]<W2obs && W2obs<W2.quantiles[row,5]) print("0.005 < p-value < 0.010")
          else if (W2.quantiles[row,5]<W2obs && W2obs<W2.quantiles[row,6]) print("0.001 < p-value < 0.005")
          else if (W2obs>W2.quantiles[row,6]) print("p-value < 0.001")
          }
       else stop("argument 'data' must be numeric")
}

