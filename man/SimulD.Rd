\name{SimulD}
\alias{SimulD}
\title{ Algorithm to derive the marginal prior predictive distribution of the EDF stadistic D }
\description{ This function approximates the predictive prior distribution of statistic D for the SN model. This is achieved by integrating out gamma with respect to the Jeffreys' prior distribution. }
\usage{ SimulD(n, nrep) }
\arguments{
  \item{n}{ size }
  \item{nrep}{ number of draws from the prior predictive distribution for gamma used to approximate \code{h(t)} }
 }
\details{ \code{h(t)} is the marginal prior predictive distribution of D obtained by integrating gamma with respect to the Jeffreys'prior. To approximate \code{h(t)} for a particular \code{n} we used M=1000000 draws from the prior using the following 3 steps algorithm:\cr

\code{Step 1} Draw gamma(1,..,m,...M) approx pi(gamma), for M=1000000;\cr
\code{Step 2} for each gamma(m) generate a random sample of size n from the SN model with mu=0 sigma=1 and gamma=gamma(m);\cr
\code{Step 3} for the mth sample calculate the stadistic D on the random sample.\cr

Although the Kolmogorov-Smirnov test in \code{ks.sn} makes use of the approximated quantiles for a set of sample sizes, with this function it is possible to approximate h(t) and to obtain its quantiles for any sample size n. }
\value{ \item{y }{ a vector of simulations that contains a sample of values that aproximate \code{h(t)} for a particular \code{n} } }
\references{ Cabras and Castellanos (2008) Default Bayesian goodness-of-fit tests for the skew-normal model. }
\author{ Veronica Paton Romero, Universidad Rey Juan Carlos, Spain \email{v.paton@alumnos.urjc.es} }
\seealso{ \code{\link{SimulW2}},\code{\link{prior.lambda}} }
\examples{
data(prior.lambda)
simulationsD=SimulD(5,10)
}
\keyword{ distribution }

