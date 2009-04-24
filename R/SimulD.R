`SimulD` <-
function(n,nrep){
           data(prior.lambda)
           if (n>0 && nrep>0){
                lambdas=sample(prior.lambda,nrep)
                gammas=lambda.to.gamma(lambdas)
                simulations=numeric(length(gammas))
                for (i in 1:nrep){
                     parameters=c(0,1,gammas[i])
                     dp=cp.to.dp(parameters)
                     data=rsn(n,dp[1],dp[2],dp[3])
                     simulations[i]=D(data)
                }
                return(simulations)
            }
            else stop("Arguments 'n' and 'nrep' must be greater than 0 ")
}

