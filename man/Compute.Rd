\name{Compute}
\alias{Compute}

\title{Rcmdr Compute Dialog}
\description{
  The compute dialog is used to compute new variables.
}

\details{
The name of the new variable must be
a valid R object name (consisting only of upper and lower-case letters, numerals, and
periods, and not starting with a numeral).

Enter an R expression in the box at the right. The expression is evaluated using the active data set. 

Note that if this expression is later
re-executed from the log window, it will be evaluated in the global environment; when the
expression is re-evaluated, the active data set must be attached to the search path 
and the variables in the expression must not be masked by other variables of the same name.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[base]{Arithmetic}}}

\keyword{manip}
