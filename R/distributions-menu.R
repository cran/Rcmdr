# Distributions menu dialogs

# last modified 5 Dec 03 by J. Fox

normalQuantiles <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Normal Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    muVar <- tclVar("0")
    muEntry <- tkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            tkmessageBox(message="No probabilities specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            normalQuantiles()
            return()
            }
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("qnorm(c(", quantiles, "), mean=", mu, 
            ", sd=", sigma, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qnorm)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="mu (mean)"), muEntry, sticky="e")
    tkgrid(tklabel(top, text="sigma (standard deviation)"), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(quantilesEntry)
    tkwait.window(top)
    }

normalProbabilities <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Normal Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    muVar <- tclVar("0")
    muEntry <- tkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            tkmessageBox(message="No values specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            normalProbabilities()
            return()
            }
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("pnorm(c(", probabilities, "), mean=", mu, 
            ", sd=", sigma, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pnorm)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="mu (mean)"), muEntry, sticky="e")
    tkgrid(tklabel(top, text="sigma (standard deviation)"), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(probabilitiesEntry)
    tkwait.window(top)
    }

tQuantiles <- function(){
    top <- tktoplevel()
    tkwm.title(top, "t Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            tkmessageBox(message="No probabilities specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tQuantiles()
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tQuantiles()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("qt(c(", quantiles, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qt)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(quantilesEntry)
    tkwait.window(top)
    }
    
tProbabilities <- function(){
    top <- tktoplevel()
    tkwm.title(top, "t Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        df <- as.numeric(tclvalue(dfVar))
        if ("" == probabilities) {
            tkmessageBox(message="No values specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tProbabilities()
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tProbabilities()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("pt(c(", probabilities, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pt)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(probabilitiesEntry)
    tkwait.window(top)
    }

chisqQuantiles <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Chi-Squared Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            tkmessageBox(message="No probabilities specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            chisqQuantiles()
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            chisqQuantiles()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("qchisq(c(", quantiles, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qchisq)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(quantilesEntry)
    tkwait.window(top)
    }
    
chisqProbabilities <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Chi-Squared Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            tkmessageBox(message="No values specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            chisqProbabilities()
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            chisqProbabilities()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("pchisq(c(", probabilities, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pchisq)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(probabilitiesEntry)
    tkwait.window(top)
    }

FQuantiles <- function(){
    top <- tktoplevel()
    tkwm.title(top, "F Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    df1Var <- tclVar("")
    df1Entry <- tkentry(top, width="6", textvariable=df1Var)
    df2Var <- tclVar("")
    df2Entry <- tkentry(top, width="6", textvariable=df2Var)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            tkmessageBox(message="Probabilities not specified", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FQuantiles()
            return()
            }
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
        if (is.na(df1) || is.na(df2)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FQuantiles()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("qf(c(", quantiles, "), df1=", df1, 
            ", df2=", df2, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qf)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Numerator degrees of freedom"), df1Entry, sticky="e")
    tkgrid(tklabel(top, text="Denominator degrees of freedom"), df2Entry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(quantilesEntry)
    tkwait.window(top)
    }
    
FProbabilities <- function(){
    top <- tktoplevel()
    tkwm.title(top, "F Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    df1Var <- tclVar("")
    df1Entry <- tkentry(top, width="6", textvariable=df1Var)
    df2Var <- tclVar("")
    df2Entry <- tkentry(top, width="6", textvariable=df2Var)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            tkmessageBox(message="Values not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FProbabilities()
            return()
            }
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
        if (is.na(df1) || is.na(df2)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FProbabilities()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("pf(c(", probabilities, "), df1=", df1, 
            ", df2=", df2, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pf)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Numerator degrees of freedom"), df1Entry, sticky="e")
    tkgrid(tklabel(top, text="Denominator degrees of freedom"), df2Entry, sticky="e")    
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(probabilitiesEntry)
    tkwait.window(top)
    }

binomialQuantiles <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Binomial Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    trialsVar <- tclVar("")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        trials <- as.numeric(tclvalue(trialsVar))
        prob <- as.numeric(tclvalue(probVar))
        if ("" == quantiles) {
            tkmessageBox(message="Probabilities not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialQuantiles()
            return()
            }
        if (is.na(trials)) {
            tkmessageBox(message="Binomial trials not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialQuantiles()
            return()
            }
        if (is.na(prob)) {
            tkmessageBox(message="Probability of success not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialQuantiles()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("qbinom(c(", quantiles, "), size=", trials, 
            ", prob=", prob, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(qbinom)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(quantilesEntry)
    tkwait.window(top)
    }
    
binomialProbabilities <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Cumulative Binomial Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    trialsVar <- tclVar("")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        trials <- as.numeric(tclvalue(trialsVar))
        prob <- as.numeric(tclvalue(probVar))
        if ("" == probabilities) {
            tkmessageBox(message="Values not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialProbabilities()
            return()
            }
        if (is.na(trials)) {
            tkmessageBox(message="Binomial trials not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialProbabilities()
            return()
            }
        if (is.na(prob)) {
            tkmessageBox(message="Probability of success not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialProbabilities()
            return()
            }
        tail <- tclvalue(tailVar)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("pbinom(c(", probabilities, "), size=", trials, 
            ", prob=", prob, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(pbinom)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")    
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(probabilitiesEntry)
    tkwait.window(top)
    }

binomialMass <- function(){
    checkTrials <- function(trials){
        tkmessageBox(message=paste("Number of trials", trials, "is large.\nCreate long output?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Binomial Probabilities")
    trialsVar <- tclVar("")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    onOK <- function(){
        trials <- as.numeric(tclvalue(trialsVar))
        if (is.na(trials)) {
            tkmessageBox(message="Binomial trials not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialMass()
            return()
            }
        if (trials > 50){
            if ("no" == tclvalue(checkTrials(trials))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                binomialMass()
                return()
                }
            }
        prob <- as.numeric(tclvalue(probVar))
        if (is.na(prob)) {
            tkmessageBox(message="Probability of success not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialMass()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("data.frame(Pr=dbinom(0:", trials, ", size=", trials, 
            ", prob=", prob, "))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- 0:", trials, sep=""))
#        justDoIt(paste("rownames(.Table) <<- 0:", trials, sep=""))
        justDoIt(paste("rownames(.Table) <- 0:", trials, sep=""))
        justDoIt(paste("print(", logger(".Table"), ")", sep=""))
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)       
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dbinom)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(trialsEntry)
    tkwait.window(top)
    }

PoissonMass <- function(){
    checkRange <- function(range){
        tkmessageBox(message=paste("Range of values over which to plot,", range, ", is large.\nCreate long output?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Poisson Probabilities")
    meanVar <- tclVar("")
    meanEntry <- tkentry(top, width="6", textvariable=meanVar)
    onOK <- function(){
        mean <- as.numeric(tclvalue(meanVar))
        if (is.na(mean)) {
            tkmessageBox(message="Poisson mean not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            PoissonMass()
            return()
            }
        min <- qpois(.00005, lambda=mean)
        max <- qpois(.99995, lambda=mean)
        range <- max - min
        if (range > 50){
            if ("no" == tclvalue(checkRange(range))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                PoissonMass()
                return()
                }
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("data.frame(Pr=round(dpois(", min, ":", max, ", lambda=", mean, "), 4))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- ", min, ":", max, sep=""))
#        justDoIt(paste("rownames(.Table) <<- ", min, ":", max, sep=""))
        justDoIt(paste("rownames(.Table) <- ", min, ":", max, sep=""))
        justDoIt(paste("print(", logger(".Table"), ")", sep=""))
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)       
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dpois)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Mean"), meanEntry, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(meanEntry, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(meanEntry)
    tkwait.window(top)
    }
