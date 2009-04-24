\name{prior.lambda}
\alias{prior.lambda}
\docType{data}
\title{ Vector of simulations from the prior distribution for lambda }
\description{ This vector contains values simulated from the Jeffrey's prior for lambda.  }
\usage{ data(prior.lambda) }
\format{ A vector of length 1000000 }
\details{ Jeffrey's prior has been simulated using a numerical approximation of the prior density and through the inverse method.  }
\references{ Cabras and Castellanos (2008) Default Bayesian goodness-of-fit tests for the skew-normal model.  }
\examples{
data(prior.lambda)
lambdas=sample(prior.lambda, 10000)
}
\keyword{ datasets }
