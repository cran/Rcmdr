# last modified 8 May 04 by J. Fox

.onAttach <- function(...){
    cat("\nRcmdr Version 0.9-8\n")
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

    rgl.warn <- options("Rcmdr")[[1]]$rgl.warn
    if (is.null(rgl.warn)) rgl.warn <- TRUE
    if (.Platform$OS.type == "windows" && (!rgl) && rgl.warn)
        tkmessageBox(message=paste("The rgl package is missing.\n", 
            "The Rcdmr will work without rgl,\n",
            "but you will not be able to draw 3D scatterplots.\n",
            "If you can't find a Windows binary for the package on CRAN\n",
            "You can get one from the author's web site,\n",
            "at <http://wsopuppenkiste.wiso.uni-goettingen.de/~dadler/rgl/>.\n",
            "To suppress this message at Rcmdr startup, set\n",
            "0ptions(Rcmdr) <- list(rgl.warn=FALSE)"),
            icon="warning", type="ok")
            
    }
