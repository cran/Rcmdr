\name{colPercents}
\alias{colPercents}
\alias{rowPercents}

\title{Row and Column Percentage Tables}
\description{
  Percentage a matrix or higher-dimensional array of frequency counts by
  rows or columns.
}
\usage{
colPercents(tab, digits=1)
rowPercents(tab, digits=1)
}

\arguments{
  \item{tab}{a matrix or higher-dimensional array of frequency counts.}\
  \item{digits}{number of places to the right of the decimal place for percentages.}
}

}
\value{
  Returns an array of the same size and shape as \code{tab} percentaged by
  rows or columns, plus rows or columns of totals and counts.
}

\author{John Fox \email{jfox@mcmaster.ca}}


\keyword{misc}

