\name{W2}
\alias{W2}
\title{ The Kolmogorov-Smirnov statistic W2 }
\description{ This statistic measures the difference between the empirical distribution, Fn, and a theoretical one, F, which must be some element of the SN class. It is only valid for continuos variables. }
\usage{ W2(data) } 
\arguments{ \item{data}{ a numeric vector with observed data } }
\details{ Central parametrization is used, and the unknown parameters are estimated by: MLE of skewness, sample mean for mu, and sample standard desviation for sigma. }
\value{ \item{x }{ a numeric value with the disparity between Fn and F } }
\references{ Cabras and Castellanos (2008) Default Bayesian goodness-of-fit tests for the skew-normal model. }
\author{ Veronica Paton Romero, Universidad Rey Juan Carlos, Spain \email{v.paton@alumnos.urjc.es} }
\seealso{ \code{\link{W2}}, \code{\link{prior.lambda}} }
\examples{ 
data(ais, package="sn")
attach(ais)
data=bmi
W2.data=W2(data)

}
\keyword{ distribution }

