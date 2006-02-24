# last modified 1 December 05 by J. Fox

.onAttach <- function(...){
    Commander()
    cat(gettext("\nRcmdr Version", domain="R-Rcmdr"), getRcmdr("RcmdrVersion"), "\n")
    }

.onLoad <- function(...){
    packagesAvailable <- function(packages){
        sapply(sapply(packages, .find.package, quiet=TRUE), 
            function(x) length(x) != 0)
        }
    save.options <- options(warn=-1)
    on.exit(options(save.options))
    tcltk <- require(tcltk)
    if (!tcltk) stop(gettext("The tcltk package is absent. The R Commander cannot function.", domain="R-Rcmdr"))
    required.packages <- rev(c("abind", "car", "effects", "foreign", "grid", "lattice", "lmtest", 
        "MASS", "mgcv", "multcomp", "nlme", "nnet", "relimp"))
    packages.to.load <- options("Rcmdr")[[1]]$load.at.startup
    if (is.null(packages.to.load)) packages.to.load <- "car"
    for (package in packages.to.load){ 
        if (length(.find.package(package, quiet=TRUE)) != 0) 
            require(package, character.only=TRUE)
        }
    check <- options("Rcmdr")[[1]]$check.packages
    if (length(check) > 0 && !check) return()
    packages.to.check <- union(required.packages, packages.to.load)
    available.packages <- packagesAvailable(packages.to.check)
    missing.packages <- packages.to.check[!available.packages]
    if (any(!available.packages)) {
        response <- tkmessageBox(message=paste(gettext("The following packages used by Rcmdr are missing:\n", domain="R-Rcmdr"),
                            paste(missing.packages, collapse=", "), 
                            gettext("\nWithout these packages, some features will not be available.", domain="R-Rcmdr"),
                            gettext("\nInstall these packages?", domain="R-Rcmdr")), 
                        icon="error", type="yesno")
        if (tclvalue(response) == "yes") {
            top <- tktoplevel(borderwidth=10)
            tkwm.title(top, gettext("Install Missing Packages", domain="R-Rcmdr"))
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
            browseButton <- tkbutton(directoryFrame, text=gettext("Browse...", domain="R-Rcmdr"), width="12", command=onBrowse, borderwidth=3)
            locationField <- tkentry(directoryFrame, width="20", textvariable=directoryVariable)
            locationScroll <- tkscrollbar(directoryFrame, orient="horizontal",
                repeatinterval=5, command=function(...) tkxview(locationField, ...))
            tkconfigure(locationField, xscrollcommand=function(...) tkset(locationScroll, ...))
            tkgrid(tklabel(top, text=gettext("Install Packages From:", domain="R-Rcmdr"), fg="blue"), sticky="nw")
            tkgrid(tklabel(directoryFrame, text=gettext("Specify package  \ndirectory:", domain="R-Rcmdr"), justify="left"), 
                locationField, sticky="w")
            tkgrid(browseButton, locationScroll, sticky="w")
            tkgrid(locationScroll, sticky="ew")
            tkgrid(tklabel(locationFrame, text="CRAN"), CRANbutton, sticky="w")
#            tkgrid(tklabel(locationFrame, text="Bioconductor"), BioconductorButton, sticky="w")
            tkgrid(tklabel(locationFrame, text=gettext("Local package directory\n(must include PACKAGES index file)", domain="R-Rcmdr"), 
                justify="left"), localButton, directoryFrame, sticky="nw")
            tkgrid(locationFrame, sticky="w")
            tkgrid(tklabel(top, text=""))
            onOK <- function(){
                errorMessage <- function() tkmessageBox(message=paste(
                    gettext("The following packages were not found at the specified location:\n", domain="R-Rcmdr"),
                    paste(missing.packages[!present], collapse=", ")),  icon="warning", type="ok")
                tkgrab.release(top)
                tkdestroy(top)
                location <- tclvalue(locationVariable)
                if (location == "CRAN") {
                    packages <- utils:::CRAN.packages()[,1]
                    present <- missing.packages %in% packages
                    if (!all(present)) errorMessage()
                    if (!any(present)) return()
                    utils:::install.packages(missing.packages[present], dependencies=TRUE, lib=.libPaths()[1])
                    }
#                else if (location == "Bioconductor") {
#                    packages <- CRAN.packages(CRAN=getOption("BIOC"))[,1]
#                    present <- missing.packages %in% packages
#                    if (!all(present)) errorMessage()
#                    install.packages(missing.packages[present], lib=.libPaths()[1],
#                        CRAN=getOption("BIOC"))
#                    }
                else {
                    directory <- paste("file:", tclvalue(directoryVariable), sep="")
                    packages <- utils:::CRAN.packages(contriburl=directory)[,1]
                    present <- missing.packages %in% packages
                    if (!all(present)) errorMessage()
                    if (!any(present)) return()
                    utils:::install.packages(missing.packages[present], contriburl=directory,
                        dependencies=TRUE, lib=.libPaths()[1])
                    }
                }
            onCancel <- function(){
                tkgrab.release(top)
                tkdestroy(top)
                return()
                }
            onHelp <- function() help("install.packages")
            buttonsFrame <- tkframe(top)
            OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active",
                    borderwidth=3)
            cancelButton <- tkbutton(buttonsFrame, text=gettext("Cancel", domain="R-Rcmdr"), fg="red", width="12", command=onCancel,
                    borderwidth=3)
            helpButton <- tkbutton(buttonsFrame, text=gettext("Help", domain="R-Rcmdr"), width="12", command=onHelp, borderwidth=3)
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
        }           
    }
