% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcorrectar1.R
\name{f.correct.ar1}
\alias{f.correct.ar1}
\title{Corrects the bias of autoregressive coefficients for AR(1)}
\usage{
f.correct.ar1(phi = NULL, method = NULL, n = NULL, x = NULL)
}
\arguments{
\item{phi}{integer for the autoregressive coeeficient estimate}

\item{method}{character string specifying the used method to estimate the autoregressive coefficients. Needs to be one of 'yw', 'mle', 'burg', 'cmle'
'yw': Yule-Walker, 'mle': Maximum Likelihood, 'burg': Burg's algorithm, 'cmle': Conditional Maximum Likelhood")}

\item{n}{integer for the length of the time series. Needs to be between 10 and 50.}

\item{x}{vector for the time series for which parameters are to be estimated, in case no previous algorithm has been made.
The default method to estimate is mle if none is specified.}
}
\value{
The bias corrected \code{phi1} and associated confidence intervals.
}
\description{
Corrects the bias of autoregressive coefficients for AR(1)
}
\keyword{internal}
