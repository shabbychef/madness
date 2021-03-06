% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max.r
\name{max}
\alias{max}
\alias{max,madness-method}
\alias{max,madness-class}
\alias{min,madness-method}
\alias{min}
\alias{min,madness-class}
\title{Maxima and Minima}
\usage{
\S4method{max}{madness}(x, ..., na.rm = FALSE)

\S4method{min}{madness}(x, ..., na.rm = FALSE)
}
\arguments{
\item{x}{\code{madness} object arguments.}

\item{...}{\code{madness} object arguments.}

\item{na.rm}{a logical indicating whether missing values should be
    removed.}
}
\description{
Return the maxima and minima of the input values.
}
\details{
\code{max} and \code{min} return the maximum or minimum of \emph{all} the
values present in their arguments.

If \code{na.rm} is \code{FALSE} and \code{NA} value in any of the arguments
will cause a value of \code{NA} to be returned, otherwise \code{NA} values are
ignored.

The minimum and maximum of a numeric empty set are \code{+Inf} and 
\code{-Inf} (in this order!) which ensures \emph{transitivity}, e.g.,
\code{min(x1, min(x2)) == min(x1, x2)}.  For numeric \code{x} 
\code{max(x) == -Inf} and \code{min(x) == +Inf} whenever 
\code{length(x) == 0} (after removing missing values if requested).
}
\author{
Steven E. Pav \email{shabbychef@gmail.com}
}
