\name{reliability}
\alias{reliability}

\title{Reliability of a Composite Scale}
\description{
  Calculates Cronbach's alpha and standardized alpha (lower bounds on reliability)
  for a composite (summated-rating) scale. Standardized alpha is for the sum of the
  standardized items. In addition, the function calculates alpha and standardized
  alpha for the scale with each item deleted in turn, and computes the correlation
  between each item and the sum of the other items.
}

\usage{
reliability(S)
}

\arguments{
  \item{S}{the covariance matrix of the items}
}

}
\value{
    The function does not return a useful value; it is called
    for its side effect -- to print the results.
}

\references{
    N. Cliff (1986) Psychological testing theory. Pp. 343--349 in
    S. Kotz and N. Johnson, eds., \emph{Encyclopedia of Statistical Sciences, Vol. 7}.
    Wiley.}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[base]{cov}}}

\examples{
data(DavisThin)
reliability(cov(DavisThin))
}
\keyword{misc}
