\name{Scatter3DDialog}
\alias{Scatter3DDialog}
\alias{Scatter3D}
\title{Rcmdr 3D Scatterplot Dialog}

\description{
  This dialog sets up a call to the \code{\link{scatter3d}} function to draw a
  three-dimensional scatterplot.  
}

\details{
The explanatory variables provide the "horizontal" and "out-of-screen" axes of the scatterplot,
the response variable provides the "vertical" axis.

Data points are represented as spheres or points,
depending upon the number of observations. 
  
Several regression surfaces can be
plotted: a linear least-squares surface; a full quadratic least-squares surface
with squared and cross-product terms; a "smooth" regression surface --- either a
smoothing spline, if no degrees of freedom are specified (in which case the
\code{\link[mgcv]{gam}} function selects the df by generalized cross validation),
or a fixed-df regression spline; an additive-regression surface (also fit by \code{gam}),
with either smoothing spline or regression spline components (again selected according
to the specification of degrees of freedom). If only one surface is fit, then residuals are
plotted as red (negative) and green (positive) lines from the surface to the points.

You can specify a factor defining groups by pressing the \emph{Plot by groups} button. A
separate surface or set of surfaces is plotted for each level of the groups factor. These
surfaces can be constrained to be parallel.
  
The completed plot can be manipulated with the mouse: Click, hold, drag the left mouse button to rotate
the display; click, hold, and drag the right button to zoom in and out.

You have to install the \code{rgl} and \code{mgcv} packages to produce 3D plots. The \code{rgl}
package is not yet available on CRAN,
but can be obtained at \url{http://wsopuppenkiste.wiso.uni-goettingen.de/~dadler/rgl/};
the \code{mgcv} package is one of the recommended R packages.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link{scatter3d}}, \code{\link[rgl]{rgl.open}}, \code{\link[mgcv]{gam}}}

\keyword{hplot}
