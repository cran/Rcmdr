# Statistics Menu dialogs

# last modified 27 Jan 04 by J. Fox

    # Dimensional-analysis menu
    
Reliability <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 3){
        tkmessageBox(message="There fewer than 3 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Scale Reliability")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        if (3 > length(x)) {
            tkmessageBox(message="Fewer than 3 variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Reliability()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        x <- paste('"', x, '"', sep="")
        doItAndPrint(paste("reliability(cov(", .activeDataSet, "[,c(", paste(x, collapse=","),
            ')], use="complete.obs"))', sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(reliability)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick three or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

principalComponents <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 2){
        tkmessageBox(message="There fewer than 2 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Principal Components Analysis")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    optionsFrame <- tkframe(top)
    correlationsVariable <- tclVar("1")
    correlationsCheckBox <- tkcheckbutton(optionsFrame, variable=correlationsVariable)
    screeplotVariable <- tclVar("0")
    screeplotCheckBox <- tkcheckbutton(optionsFrame, variable=screeplotVariable)
    addPCVariable <- tclVar("0")
    addPCCheckBox <- tkcheckbutton(optionsFrame, variable=addPCVariable)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        nvar <- length(x)
        correlations <- tclvalue(correlationsVariable)
        subset <- tclvalue(subsetVariable)
        screeplot <- tclvalue(screeplotVariable)
        addPC <- tclvalue(addPCVariable)
        if (2 > length(x)) {
            tkmessageBox(message="Fewer than 2 variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            principalComponents()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" else paste(", subset=", subset, sep="")
        correlations <- if (correlations == "1") "TRUE" else "FALSE"
        command <- paste("princomp(~", paste(x, collapse="+"), ", cor=", correlations,
            ", data=", .activeDataSet, subset, ")", sep="")
        assign(".PC", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".PC <- ", command, sep=""))
        doItAndPrint("unclass(loadings(.PC))  # component loadings")
        doItAndPrint(".PC$sd^2  # component variances")
        if (screeplot == "1") {
            justDoIt("screeplot(.PC)")
            logger("screeplot(.PC)")
            }
        if (addPC == "1") {
            if (is.element("PC1", .variables)) {
                if ("no" == tclvalue(checkReplace("PC1"))){
                    if (.grab.focus) tkgrab.release(top)
                    tkdestroy(top)
                    remove(.PC, envir=.GlobalEnv)   
                    logger("remove(.PC)")
                    return()
                    }
                }
            for(i in 1:nvar){
#                justDoIt(paste(.activeDataSet, "$PC", i, " <<- .PC$scores[,", i, "]", sep=""))
                justDoIt(paste(.activeDataSet, "$PC", i, " <- .PC$scores[,", i, "]", sep=""))
                logger(paste(.activeDataSet, "$PC", i, " <- .PC$scores[,", i, "]", sep=""))
                }
            activeDataSet(.activeDataSet)
            remove(.PC, envir=.GlobalEnv)   
            }
        logger("remove(.PC)")
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(princomp)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick two or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Analyze correlation matrix"), 
        correlationsCheckBox, sticky="e")
    tkgrid(tklabel(optionsFrame, text="Screeplot"), screeplotCheckBox, sticky="e")
    tkgrid(tklabel(optionsFrame, text="Add principal components\nto data set", justify="left"),
        addPCCheckBox, sticky="ne")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(correlationsCheckBox, sticky="w")
    tkgrid.configure(screeplotCheckBox, sticky="w")
    tkgrid.configure(addPCCheckBox, sticky="w")   
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

factorAnalysis <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 3){
        tkmessageBox(message="There fewer than 3 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Factor Analysis")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    optionsFrame <- tkframe(top)
    nfactorVariable <- tclVar("1")
    nfactorEntry <- tkentry(optionsFrame, width="2", textvariable=nfactorVariable)
    checkFrame <- tkframe(top)
    rotationVariable <- tclVar("varimax")
    rotationFrame <- tkframe(checkFrame)
    noRotateButton <- tkradiobutton(rotationFrame, variable=rotationVariable, value="none")
    varimaxButton <- tkradiobutton(rotationFrame, variable=rotationVariable, value="varimax")
    promaxButton <- tkradiobutton(rotationFrame, variable=rotationVariable, value="promax")
    scoresVariable <- tclVar("none")
    scoresFrame <- tkframe(checkFrame)
    noScoresButton <- tkradiobutton(scoresFrame, variable=scoresVariable, value="none")
    bartlettButton <- tkradiobutton(scoresFrame, variable=scoresVariable, value="Bartlett")
    regressionButton <- tkradiobutton(scoresFrame, variable=scoresVariable, value="regression")
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        nvar <- length(x)
        nfactor <- as.numeric(tclvalue(nfactorVariable))
        subset <- tclvalue(subsetVariable)
        rotation <- tclvalue(rotationVariable)
        scores <- tclvalue(scoresVariable)
        if (3 > length(x)) {
            tkmessageBox(message="Fewer than 3 variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            factorAnalysis()
            return()
            }
        f <- function(k, p) ((p - k)^2 - p - k)^2
        max.factors <- floor(optimize(f, c(0, nvar), tol=.0001, p=nvar)$minimum)
        if (nfactor > max.factors) {
            if (max.factors > 1)
                tkmessageBox(message=paste("Number of factors must be between 1 and ",
                    max.factors, ".", sep=""),
                    icon="error", type="ok")
            else
                tkmessageBox(message="Number of factors cannot exceed 1",
                    icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            factorAnalysis()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" else paste(", subset=", subset, sep="")
        command <- paste("factanal(~", paste(x, collapse="+"), ", factors=", nfactor, ', rotation="', rotation,
            '", scores="', scores, '", data=', .activeDataSet, subset, ")", sep="")
        assign(".FA", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".FA <- ", command, sep=""))
        doItAndPrint(".FA")
        if (scores != "none") {
            if (is.element("F1", .variables)) {
                if ("no" == tclvalue(checkReplace("F1"))){
                    if (.grab.focus) tkgrab.release(top)
                    tkdestroy(top)
                    remove(.FA, envir=.GlobalEnv)   
                    logger("remove(.FA)")
                    return()
                    }
                }
            for(i in 1:nfactor){
                justDoIt(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", i, "]", sep=""))
                logger(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", i, "]", sep=""))
                }
            activeDataSet(.activeDataSet)
            }
        logger("remove(.FA)")
        remove(.FA, envir=.GlobalEnv)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(factanal)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick three or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Number of factors:"),
        nfactorEntry, sticky="w")
    tkgrid(tklabel(rotationFrame, text="Factor Rotation", fg="blue"), sticky="w")
    tkgrid(tklabel(rotationFrame, text="None"), noRotateButton, sticky="w")
    tkgrid(tklabel(rotationFrame, text="Varimax"), varimaxButton, sticky="w")
    tkgrid(tklabel(rotationFrame, text="Promax"), promaxButton, sticky="w")
    tkgrid(tklabel(scoresFrame, text="Factor Scores", fg="blue"), sticky="w")
    tkgrid(tklabel(scoresFrame, text="None"), noScoresButton, sticky="w")
    tkgrid(tklabel(scoresFrame, text="Bartlett's method"), bartlettButton, sticky="w")
    tkgrid(tklabel(scoresFrame, text="Regression method"), regressionButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(rotationFrame, scoresFrame, sticky="w")
    tkgrid(checkFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), 
        helpButton,sticky="w")
    tkgrid(buttonsFrame,  sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
