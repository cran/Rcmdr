\name{scatter3d}
\alias{scatter3d}
\title{Three-Dimensional Scatterplots}

\description{
  This function uses the \code{rgl} package to draw 3D scatterplots
  with various regression surfaces.
}

\usage{
scatter3d(x, y, z, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
    zlab=deparse(substitute(z)), revolutions=0, bg.col=c("black", "white"), axis.col=NULL,
    surface.col=c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"), 
    neg.res.col="red", pos.res.col="green", point.col="yellow",
    text.col=axis.col, grid.col=if (bg.col == "white") "black" else "gray", 
    fogtype=c("exp2", "linear", "exp", "none"), 
    residuals=(length(fit) == 1), surface=TRUE, grid=TRUE, df.smooth=NULL, df.additive=NULL,
    sphere.size=1, threshold=0.01, speed=1, fov=60, 
    fit="linear", groups=NULL, parallel=TRUE, model.summary=FALSE)
}

\note{
You have to install the \code{rgl} and \code{mgcv} packages to produce 3D plots. 
}

\arguments{
  \item{x}{variable for horizontal axis.}
  \item{y}{variable for vertical axis (response).}
  \item{z}{variable for out-of-screen axis.}
  \item{xlab, ylab, zlab}{axis labels.}
  \item{revolutions}{number of full revolutions of the display.}
  \item{bg.col}{background colour; one of \code{"white"}, \code{"black"}.}
  \item{axis.col}{colour for axes; default is \code{"white"} for black background, 
    \code{"black"} for white background.}
  \item{surface.col}{vector of colours for regression planes, 
    used in the order specified by \code{fit}.}
  \item{neg.res.col, pos.res.col}{colours for lines representing negative 
    and positive residuals.}
  \item{point.col}{colour of points.}
  \item{text.col}{colour of axis labels.}
  \item{grid.col}{colour of grid lines on the regression surface(s).}
  \item{fogtype}{type of fog effect; one of \code{"exp2"}, \code{"linear"}, 
    \code{"exp"}, \code{"none".}}
  \item{residuals}{plot residuals (\code{TRUE} or \code{FALSE}); available only 
    when there is one surface plotted.}
  \item{surface}{plot surface(s) (\code{TRUE} or \code{FALSE}).}
  \item{grid}{plot grid lines on the regression surface(s) (\code{TRUE} or \code{FALSE}).}
  \item{df.smooth}{degrees of freedom for the two-dimensional smooth regression surface;
    if \code{NULL} (the default), the \code{\link[mgcv]{gam}} function will select the degrees of freedom
    for a smoothing spline by generalized cross-validation; if a positive number, a fixed
    regression spline will be fit with the specified degrees of freedom.}
  \item{df.additive}{degrees of freedom for each explanatory variable in an additive regression;
    if \code{NULL} (the default), the \code{gam} function will select degrees of freedom 
    for the smoothing splines by generalized cross-validation; if a positive number
    or a vector of two positive numbers, fixed
    regression splines will be fit with the specified degrees of freedom for each term.}
  \item{sphere.size}{relative sizes of spheres representing points; 
    the actual size is dependent on the number of observations.}
  \item{threshold}{if the actual size of the spheres is less than the threshold, 
    points are plotted instead.}
  \item{speed}{relative speed of revolution of the plot.}
  \item{fov}{field of view (in degrees); controls degree of perspective.}
  \item{fit}{one or more of \code{"linear"}, \code{"quadratic"}, \code{"smooth"}, 
    \code{"additive"}; to display fitted surface(s); partial matching is supported -- 
    e.g., \code{c("lin", "quad")}.}
  \item{groups}{if \code{NULL} (the default), no groups are defined; if a factor, a different surface
    or set of surfaces is plotted for each level of the factor; in this event, the colours in 
    \code{plane.col} are used successively for the points, surfaces, and residuals corresponding to
    each level of the factor.}
  \item{parallel}{when plotting surfaces by \code{groups}, should the surfaces be constrained to be
    parallel? A logical value, with default \code{TRUE}.}
  \item{model.summary}{print summary or summaries of the model(s) fit 
    (\code{TRUE} or \code{FALSE}).}
}

\value{
  Does not return a useful value; used for its side-effect of
  creating a 3D scatterplot.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link[rgl]{rgl.open}}, \code{\link[mgcv]{gam}}}

\examples{
    \dontrun{
data(state)
State.x77 <- as.data.frame(state.x77)
with(State.x77, scatter3d(Income, Murder, Illiteracy))
with(State.x77,  scatter3d(Income, Murder, Illiteracy, fit=c("linear", "quadratic")))
    }
}

\keyword{hplot}
