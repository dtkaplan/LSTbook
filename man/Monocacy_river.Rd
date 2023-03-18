\name{Monocacy_river}
\alias{Monocacy_river}
\docType{data}
\title{
Precipitation and the consequent river run-off
}
\description{
When it rains, some of the water is absorbed into the ground or quickly evaporates. Some of the water runs off (the "runoff") and is collected by streams and rivers. These data, from a 1964 reference on water resource management, were measured after 25 storms in the basin of the Monocacy River at Jug Bridge, Maryland, US.
}
\usage{data("Monocacy_river")}
\format{
  A data frame with 25 observations on the following 2 variables.
  \describe{
    \item{\code{precip}}{amount of rain in inches}
    \item{\code{runoff}}{runoff in inches}
  }
}

\source{
R.K. Linsley and J.B. Franzini (1964) *Water Resources Engineering* McGraw-Hill, p.68
}

\examples{
data(Monocacy_river)
}
\keyword{datasets}
