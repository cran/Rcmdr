# last modified 21 May 04 by J. Fox

.onAttach <- function(...){
    cat("\nRcmdr Version 0.9-9\n")
    Commander()
    }

.onLoad <- function(...){
    save.options <- options(warn=-1)
    on.exit(options(save.options))
    lattice <- require(lattice)
    foreign <- require(foreign)
    tcltk <- require(tcltk)
    abind <- require(abind)
    lmtest <- require(lmtest)
    multcomp <- require(multcomp)
    mvtnorm <- require(mvtnorm)
    relimp <- require(relimp)
    effects <- require(effects)
    rgl <- require(rgl)
    mgcv <- require(mgcv)
    car <- require(car)
    absent <- !c(lattice, foreign, tcltk, abind, lmtest, multcomp, mvtnorm, relimp,
        effects, rgl, mgcv, car)
    if (any(absent)) {
        cat("\nThe following packages required by Rcmdr are missing:\n")
        cat(paste(c("lattice", "foreign", "tcltk", "abind", "lmtest", "multcomp", 
            "mvtnorm", "relimp", "effects", "rgl", "mgcv", "car")[absent], collapse=", "))
        cat("\n")
        }           
    }
