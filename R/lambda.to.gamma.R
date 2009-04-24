`lambda.to.gamma` <-
function(lambda){
    mu.Z <- lambda * sqrt(2/(pi * (1 + lambda^2)))
    s.Z <- sqrt(1 - mu.Z^2)
    gamma1 <- 0.5 * (4 - pi) * (mu.Z/s.Z)^3
return(gamma1)
}

