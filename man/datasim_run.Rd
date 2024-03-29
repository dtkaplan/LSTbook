% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasim_run.R
\name{datasim_run}
\alias{datasim_run}
\title{Run a datasim simulation, producing a data frame}
\usage{
datasim_run(sim, n = 5, seed = NULL, report_hidden = FALSE)
}
\arguments{
\item{sim}{A simulation object, as produced by \code{datasim_make()}.}

\item{n}{The size of the data sample pulled from the simulation.}

\item{seed}{An integer random number seed, for reproducibility. (Or, use \code{set.seed()} before
running \code{sim_run()}.)}

\item{report_hidden}{logical. If \code{TRUE}, show the values of hidden variables (that is, variables whose name
begins with a dot)}
}
\value{
a data frame with a column for each node in the datasim object.
}
\description{
Run a datasim simulation, producing a data frame
}
