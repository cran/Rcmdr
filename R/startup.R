# last modified 9 July 03 by J. Fox

.onAttach <- function(...){
    cat("\nRcmdr Version 0.9-0\n")
    Commander()
    }

.onLoad <- function(...){
    save.options <- options(warn=-1)
    on.exit(options(save.options))
    foreign <- require(foreign)
    mva <- require(mva)
    ctest <- require(ctest)
    tcltk <- require(tcltk)
    car <- require(car)
    absent <- !c(foreign, mva, ctest, tcltk, car)
    if (any(absent)) {
        cat("\nThe following packages required by Rcmdr are missing:\n")
        cat(paste(c("foreign", "mva", "ctest", "tcltk", "car")[absent], collapse=", "))
        cat("\n")
        }
    }
