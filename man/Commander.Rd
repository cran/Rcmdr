\name{Commander}
\alias{Commander}

\title{R Commander}
\description{
  Start the R Commander GUI (graphical user interface)
}
\usage{
Commander()
}

\details{

The \code{Rcmdr} menus may be customized by editing the file \code{Rcmdr-menus.txt}, 
which resides in the menus subdirectory of the package.

The behaviour of the package may be modified by editing the file \code{Rcmdr} in the
\code{R} subdirectory of the package.

Alternatively, you can edit the source package and recompile it.

In addition, a couple of features are controlled by run-time options, set via the \code{options()}
command. These options should be set before the package is loaded. If the options are unset, which is
the usual situation, defaults are used:

\describe{
    \item{\code{Rcmdr.fontsize}}{This is the font size, in points, to be used in the log/script window,
    in recode dialogs, and in compute expressions. The default is 10. Adjust if the default font is too large
    or too small.}
    
    \item{\code{Rcmdr.tkwait}}{Set to \code{TRUE} to force the R session to wait for the Commander window 
    to exit before proceeding. This apparently prevents the session from hanging on some systems. It should
    not be necessary in Windows. Try running without setting the option first. A disadvantage of setting
    \code{options(Rcmdr.tkwait=TRUE)} is that R commands cannot be entered directly at the R prompt when the
    Commander is running. You should be able to enter commands in the log/script window, however.}    
    }

}

\author{John Fox \email{jfox@mcmaster.ca}}

\keyword{misc}
