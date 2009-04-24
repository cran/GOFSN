\name{GOFSN-package}
\alias{GOFSN-package}
\alias{GOFSN}
\docType{package}
\title{ Package GOFSN: summary information }
\description{ This package proposes a series of goodness-of-fit tests for the family of skew-normal models when all parameters are unknown. It provides functions that analyse if data are compatible with the assumed model. }
\details{
\tabular{ll}{
Package: \tab GOFSN\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-04-24\cr
License: \tab GPL (version 2 or later)\cr
LazyLoad: \tab yes\cr
}
}
\section{Functions}{ The package includes functions to analyze the fit of a univariate distribution to a proposed data set: \code{\link{D}},  \code{\link{W2}},  \code{\link{SimulD}},  \code{\link{SimulW2}},  \code{\link{ks.sn}}, \code{\link{W2.sn}}.

It also includes some objects necessary for the above functions: \code{\link{prior.lambda}},  \code{\link{D.quantiles}}, \code{\link{W2.quantiles}}. }
\section{Requeriments}{ R(>=2.7.0), sn}
\author{ Veronica Paton Romero, Universidad Rey Juan Carlos, Spain   \email{v.paton@alumnos.urjc.es} }
\keyword{ distribution }
\examples{ 
x<-c(2,5,10,4,3,8)
D.data<-D(x)
W2.data<-W2(x) }
