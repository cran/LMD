% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_monotonous.R
\name{is_monotonous}
\alias{is_monotonous}
\title{Monotonicity Check}
\usage{
is_monotonous(signal)
}
\arguments{
\item{signal}{Signal values (Numeric | vector)}
}
\value{
Boolean
}
\description{
Method for checking if signal is increasing or decreasing monotonously
}
\details{
A monotonic signal is a function that keeps increasing or decreasing as its domain variable proceeds.#'
}
\examples{
x=1:100
is_monotonous(x)
}
\references{
\url{https://pypi.org/project/PyLMD/}
}
\author{
Shubhra Prakash, \email{shubhraprakash279@gmail.com}
}
\keyword{monotonic}
\keyword{monotonous}
