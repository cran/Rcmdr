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

In addition, several features are controlled by run-time options, set via the \code{options("Rcmdr")}
command. These options should be set before the package is loaded. If the options are unset, which is
the usual situation, defaults are used. Specify options as a list of \emph{name=value} pairs. You can
set none, one, several, or all options. The available options are as follows:

\describe{
    \item{\code{log.font.size}}{The font size, in points, to be used in the log/script window,
    in recode dialogs, and in compute expressions --- that is, where a monospaced font is used. 
    The default is 10.}
    
    \item{\code{log.width}}{The width of the log window, in characters. The default is 70.}
    
    \item{\code{log.height}}{The height of the log window, in lines. The default is 15.}
    
    \item{\code{contrasts}}{Serves the same function as the general \code{contrasts} option; the default is 
    \code{c("contr.Treatment", "contr.poly")}. When the \code{Commander} exits, the \code{contrasts}
    option is returned to its pre-existing value. Note that \code{contr.Treatment} is from the \code{car} package.}
    
    \item{\code{scale.factor}}{A scaling factor to be applied to all Tk elements, such as fonts. This works well
    only in Windows. The default is \code{NULL}.}
    
    \item{\code{default.font.size}}{The size, in points, of the default font. The default is 10. Unless otherwise 
    specified (see the next item), the default font is \code{"*helvetica-medium-r-normal-*-xx*"}, where \code{xx}
    is the default font size. This option is only for non-Windows systems.}
    
    \item{\code{default.font}}{The default font, as an X11 font specification, given in a character string. 
    If specified, this value takes precedence
    over the default font size (above). This option is only for non-Windows systems.}
    
    \item{\code{tkwait}}{This option addresses a problem that, to my knowledge, is peculiar to Debian Linux. Set to
    \code{TRUE} for Debian Linux and to \code{FALSE} or ignore for other systems. An undesirable side effect of setting
    the \code{tkwait} option to \code{TRUE} is that the R session command prompt is suppressed until the Commander exits.
    One can still enter commands via the log/script window, however. In particular, there is no reason to use this option
    under Windows, and it should not be used with the Windows R GUI with buffered output.}
    }
    
}

\author{John Fox \email{jfox@mcmaster.ca}}

\keyword{misc}
