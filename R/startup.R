# last modified 22 December 03 by J. Fox

.onAttach <- function(...){
    cat("\nRcmdr Version 0.9-3\n")
    Commander()
    }

.onLoad <- function(...){
    save.options <- options(warn=-1)
    on.exit(options(save.options))
    lattice <- require(lattice)
    foreign <- require(foreign)
    mva <- require(mva)
    ctest <- require(ctest)
    tcltk <- require(tcltk)
    abind <- require(abind)
    lmtest <- require(lmtest)
    effects <- require(effects)
    car <- require(car)
    absent <- !c(lattice, foreign, mva, ctest, tcltk, abind, lmtest, effects, car)
    if (any(absent)) {
        cat("\nThe following packages required by Rcmdr are missing:\n")
        cat(paste(c("lattice", "foreign", "mva", "ctest", "tcltk", "abind", "lmtest", 
            "effects", "car")[absent], collapse=", "))
        cat("\n")
        }
    }
