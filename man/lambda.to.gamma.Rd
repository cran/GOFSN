\name{lambda.to.gamma}
\alias{lambda.to.gamma}
\title{ Converts lambda values to gamma values  }
\description{ For a given value of lambda, parameter of skewness in direct parametrization, the function finds the corresponding value of gamma. This function is necessary for other functions as \code{SimulD} and \code{SimulW2}.  }  
\usage{ lambda.to.gamma(lambda) } 
\arguments{ \item{lambda}{ A numeric vector of simulated values of lambda. } }
\value{ \item{gamma }{ A numeric vector of the corresponding gamma values. } }
\references{ Cabras and Castellanos (2008) Default Bayesian goodness-of-fit tests for the skew-normal model. }
\author{ Veronica Paton Romero, Universidad Rey Juan Carlos, Spain   \email{v.paton@alumnos.urjc.es} }
\seealso{ \code{\link{SimulD}}, \code{\link{SimulW2}} }
\examples{
data(prior.lambda)
lambdas=sample(prior.lambda,10000)
gammas=lambda.to.gamma(lambdas)
}
\keyword{ distribution }
