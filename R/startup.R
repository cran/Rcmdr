# last modified 25 May 03 by J. Fox

.onAttach <- function(...){
   library(foreign)
   library(mva)
   library(ctest)
   library(tcltk)
   library(car)
    assign(".images", paste(.path.package(package="Rcmdr")[1], "/", "bitmaps", "/", sep=""),
        envir=.GlobalEnv)
    assign(".menus", paste(.path.package(package="Rcmdr")[1], "/", "menus", "/", sep=""),
        envir=.GlobalEnv)
    assign(".activeDataSet", NULL, envir=.GlobalEnv)
    assign(".activeModel", NULL, envir=.GlobalEnv)
    assign(".logFileName", NULL, envir=.GlobalEnv)
    font <- options("Rcmdr.fontsize")[[1]]
    log.size <- if (is.null(font)) 10 else font
    assign(".logFont", tkfont.create(family="courier", size=log.size), envir=.GlobalEnv)
    cat("Rcmdr package, Version 0.8-2\n")
    cat("Current path:\n ")
    print(search())
    Commander()
    tkfocus(.commander)
    }
