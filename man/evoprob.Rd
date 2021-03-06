% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evoprob.R
\name{evoprob}
\alias{evoprob}
\title{Instance-evolving EA.}
\usage{
evoprob(
  n,
  P,
  maximize = FALSE,
  runner.fun,
  fitness.fun,
  mut.fun,
  diversity.fun = NULL,
  max.iters = 10L,
  max.time = Inf,
  logger = NULL,
  ...
)
}
\arguments{
\item{n}{[\code{integer(1L)}]\cr
Desired instance size.}

\item{P}{[\code{list}]\cr
Initial population.
This is problem dependent. Therefore, the used must provide it.}

\item{maximize}{[\code{logical(1)}]\cr
Is the goal to maximize performance values?
Defaults to \code{FALSE}.}

\item{runner.fun}{[\code{function}]\cr
Function used to run algorithms on problem instance.
Should return aggregated performance values.}

\item{fitness.fun}{[\code{function}]\cr
Fitness function.}

\item{mut.fun}{[\code{function}]\cr
Mutation operator.}

\item{diversity.fun}{[\code{function}]\cr
Diversity function, i.e. function that calculates the diversity of a set
of instances.}

\item{max.iters}{[\code{integer(1)}]\cr
Stopping condition: maximum number of iterations.}

\item{max.time}{[\code{integer(1)}]\cr
Maximum time  in seconds.
Default is \code{NULL}, i.e. the number of iterations serves as the single
termination criterion.}

\item{logger}{[\code{evoprob_logger}]\cr
Optional logger environemt (see \code{\link{init_logger}}). Possible
values for parameter \code{what} are
\describe{
  \item{iter = "numeric"}{\strong{Mandatory} iteration counter.}
  \item{time = "numeric"}{Time passed in seconds.}
  \item{P = "list"}{The population.}
  \item{fP = "list"}{The fitness values.}
  \item{div = "numeric"}{Scalar diversity measure.}
  \item{divtab = "list"}{Vector of absolute frequencies of algorithm rankings.}
}
Default is \code{NULL}, i.e., logging is not active.}

\item{...}{[any]\cr
Not used at the moment.}
}
\value{
[\code{list}]
}
\description{
To be written ...
}
