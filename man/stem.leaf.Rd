\name{stem.leaf}
\alias{stem.leaf}
\alias{print.stem.leaf}

\title{Stem-and-Leaf Display}
\description{
  Creates a classical ("Tukey-style") stem-and-leaf display.
}

\usage{
stem.leaf(data, unit, m, Min, Max,
    rule.line = c("Dixon", "Velleman", "Sturges"),
    style = c("Tukey", "bare"), trim.outliers = TRUE, depths = TRUE,
    reverse.negative.leaves = TRUE)

\method{print}{stem.leaf}(x, ...)
}

\arguments{
  \item{data}{a numeric vector.}
  \item{unit}{leaf unit, as a power of 10 (e.g., \code{100}, \code{.01});
    omit to let the function choose the unit.}
  \item{m}{number of parts (1, 2, or 5) into which each stem should be
    divided; omit to let the function choose the number of parts/stem.}
  \item{Min}{smallest non-outlying value; omit for automatic choice.}
  \item{Max}{largest non-outlying value; omit for automatic choice.}
  \item{rule.line}{the rule to use for choosing the desired number of lines
    in the display; \code{"Dixon"} = 10*log10(n); \code{"Velleman"} = 2*sqrt(n);
    \code{"Sturges"} = 1 + log2(n); the default is \code{"Dixon"}.}
  \item{style}{\code{"Tukey"} (the default) for "Tukey-style" divided stems;
    \code{"bare"} for divided stems that simply repeat the stem digits.}
  \item{trim.outliers}{if \code{TRUE} (the default), outliers are placed on \code{LO} and
    \code{HI} stems.}
  \item{depths}{if \code{TRUE} (the default), print a column of "depths" to the left of the
    stems; the depth of the stem containing the median is the stem-count enclosed in
    parentheses.}
  \item{reverse.negative.leaves}{if \code{TRUE} (the default), reverse the leaves on negative
    stems (so, e.g., the leaf 9 comes before the leaf 8, etc.).}
  \item{x}{an object of class stem.leaf to be printed.}
  \item{...}{not used: for compatibility with the generic print function.}
}

\details{
  Unlike the \code{stem} function in the \code{base} package, this function produces
  classic stem-and-leaf displays, as described in Tukey's \emph{Exploratory Data Analysis}.
  Outliers are determined using the rule for boxplots (see \code{\link[grDevices]{boxplot.stats}}).
}

\value{
  Returns on object of class stem.leaf, which normally would be printed.
}

\references{
    Tukey, J. 
    \emph{Exploratory Data Analysis.}
    Addison-Wesley, 1977.
    }

\author{Peter Wolf, slightly modified by John Fox \email{jfox@mcmaster.ca}
    with the original author's permission.}

\seealso{\code{\link[graphics]{stem}}}

\examples{
data(Prestige)
stem.leaf(Prestige$income)
}

\keyword{misc} 
