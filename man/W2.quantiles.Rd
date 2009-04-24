\name{W2.quantiles}
\alias{W2.quantiles}
\docType{data}
\title{ Matrix W2 for all sizes and quantiles }
\description{ This matrix provides critical quantiles of the prior predictive distribution \code{m(t)} for the EDF stadistic W2 and different sizes \code{n}.  }
\usage{ data(W2.quantiles) }
\format{ A data frame with 51 rows and 6 columns. Rows represent the sample size (5,6,...,49,50,60,70,80,90,100) and columns represent the critical quantiles (0.10,0.05,0.025,0.010,0.005,0.001). }
\details{ Quantiles of the prior predictive distribution for statistic W2 are approximated by Monte Carlo simulations, using 100.000 draws from this predictive distribution. Details of calculation are given in Section 3.2 from article Cabras,Castellanos (2008). } 
\references{ Cabras and Castellanos (2008) Default Bayesian goodness-of-fit tests for the skew-normal model. }
\seealso{ \code{\link{SimulW2}}, \code{\link{W2.sn}} }
\examples{
data(W2.quantiles)  
W2.size5=W2.quantiles[1,] 
}
\keyword{ datasets }
