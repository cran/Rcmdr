# last modified 19 Nov 04 by J. Fox

.onAttach <- function(...){
    cat("\nRcmdr Version 0.9-14\n")
    Commander()
    }

.onLoad <- function(...){
    save.options <- options(warn=-1)
    on.exit(options(save.options))
    tcltk <- require(tcltk)
    required.packages <- rev(c("abind", "car", "effects", "foreign", "lattice", "lmtest", "MASS", 
        "mgcv", "multcomp", "mvtnorm", "nlme", "nnet", "relimp", "rgl", "sandwich", "strucchange", "zoo"))
    for (package in required.packages) assign(package, require(package, character.only=TRUE))
    if (!tcltk) stop("The tcltk package is absent. The R Commander cannot function.")
    absent <- !sapply(required.packages, function(package) eval(parse(text=package)))
    missing.packages <- required.packages[absent]
    if (any(absent)) {
        response <- tkmessageBox(message=paste("The following packages required by Rcmdr are missing:\n",
                            paste(missing.packages, collapse=", "), "\nInstall these packages?"), 
                        icon="error", type="yesno")
        if (as.character(response) == "yes") {
            top <- tktoplevel(borderwidth=10)
            tkwm.title(top, "Install Missing Packages")
            locationFrame <- tkframe(top)
            locationVariable <- tclVar("CRAN")
            CRANbutton <- tkradiobutton(locationFrame, variable=locationVariable, value="CRAN")
#         Note: Bioconductor code not currently necessary
#            BioconductorButton <- tkradiobutton(locationFrame, variable=locationVariable, value="Bioconductor")
            localButton <- tkradiobutton(locationFrame, variable=locationVariable, value="local")
            directoryVariable <- tclVar("")
            directoryFrame <- tkframe(locationFrame)
            onBrowse <- function(){
                tclvalue(directoryVariable) <- tclvalue(tkchooseDirectory())
                }
            browseButton <- tkbutton(directoryFrame, text="Browse...", width="12", command=onBrowse, borderwidth=3)
            locationField <- tkentry(directoryFrame, width="20", textvariable=directoryVariable)
            locationScroll <- tkscrollbar(directoryFrame, orient="horizontal",
                repeatinterval=5, command=function(...) tkxview(locationField, ...))
            tkconfigure(locationField, xscrollcommand=function(...) tkset(locationScroll, ...))
            tkgrid(tklabel(top, text="Install Packages From:", fg="blue"), sticky="nw")
            tkgrid(tklabel(directoryFrame, text="Specify package  \ndirectory:", justify="left"), 
                locationField, sticky="w")
            tkgrid(browseButton, locationScroll, sticky="w")
            tkgrid(locationScroll, sticky="ew")
            tkgrid(tklabel(locationFrame, text="CRAN"), CRANbutton, sticky="w")
#            tkgrid(tklabel(locationFrame, text="Bioconductor"), BioconductorButton, sticky="w")
            tkgrid(tklabel(locationFrame, text="Local package directory\n(must include PACKAGES index file)", 
                justify="left"), localButton, directoryFrame, sticky="nw")
            tkgrid(locationFrame, sticky="w")
            tkgrid(tklabel(top, text=""))
            onOK <- function(){
                errorMessage <- function() tkmessageBox(message=paste(
                    "The following packages were not found at the specified location:\n",
                    paste(missing.packages[!present], collapse=", ")),  icon="error", type="ok")
                tkgrab.release(top)
                tkdestroy(top)
                location <- tclvalue(locationVariable)
                if (location == "CRAN") {
                    packages <- utils:::CRAN.packages()[,1]
                    present <- missing.packages %in% packages
                    if (!all(present)){
                        errorMessage()
                        stop("Missing packages.", call.=FALSE)
                        }
                    utils:::install.packages(missing.packages, lib=.libPaths()[1])
                    }
#                else if (location == "Bioconductor") {
#                    packages <- CRAN.packages(CRAN=getOption("BIOC"))[,1]
#                    present <- missing.packages %in% packages
#                    if (!all(present)){
#                        errorMessage()
#                        stop("Missing packages.", call.=FALSE)
#                        }
#                    install.packages(missing.packages., lib=.libPaths()[1],
#                        CRAN=getOption("BIOC"))
#                    }
                else {
                    directory <- paste("file:", tclvalue(directoryVariable), sep="")
                    packages <- utils:::CRAN.packages(contriburl=directory)[,1]
                    present <- missing.packages %in% packages
                    if (!all(present)){
                        errorMessage()
                        stop("Missing packages.", call.=FALSE)
                        }
                    utils:::install.packages(missing.packages, contriburl=directory, lib=.libPaths()[1])
                    }
                for (package in missing.packages) require(package, character.only=TRUE)
                }
            onCancel <- function(){
                tkgrab.release(top)
                tkdestroy(top)
                stop("Missing packages.", call.=FALSE)
                }
            onHelp <- function() help("install.packages")
            buttonsFrame <- tkframe(top)
            OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active",
                    borderwidth=3)
            cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel,
                    borderwidth=3)
            helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp, borderwidth=3)
            tkgrid(OKbutton, tklabel(buttonsFrame, text="  "), cancelButton, tklabel(buttonsFrame, text="            "), 
                helpButton, sticky="w")
            tkgrid(buttonsFrame, sticky="w")
            for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
            tkgrid.columnconfigure(top, 0, weight=0)
            .Tcl("update idletasks")
            tkwm.resizable(top, 0, 0)
            tkbind(top, "<Return>", onOK)
            tkwm.deiconify(top)
            tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        else stop("Missing packages: ", paste(missing.packages, collapse=", "), call.=FALSE)
        }           
    }
