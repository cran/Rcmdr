# Distributions  -> Plot Distributions menu dialogs

# last modified 27 Jan 04 by J. Fox

normalDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Normal Distribution")
    muVar <- tclVar("0")
    muEntry <- tkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dnorm" else "pnorm"
        min <- round(qnorm(.0005, mean=mu, sd=sigma), 3)
        max <- round(qnorm(.9995, mean=mu, sd=sigma), 3)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("seq(", min, ", ", max, ", length=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, mean=", mu, 
            ", sd=", sigma, '), xlab="x", ylab="', fun, 
            '", main=expression(paste("Normal Distribution: ", mu, " = ',
            mu, ', ", sigma, " = ', sigma, '")), type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dnorm)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="mu (mean)"), muEntry, sticky="e")
    tkgrid(tklabel(top, text="sigma (standard deviation)"), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(muEntry)
    tkwait.window(top)
    }

tDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "t Distribution")
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tDistributionPlot()
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dt" else "pt"
        min <- round(qt(.0005, df=df), 3)
        max <- round(qt(.9995, df=df), 3)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("seq(", min, ", ", max, ", length=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df=", df, 
            '), xlab="t", ylab="', fun, 
            '", main="t Distribution: df = ', df, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dt)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(dfEntry)
    tkwait.window(top)
    }

chisquareDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Chi-squared Distribution")
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            tkmessageBox(message="Degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            chisquareDistributionPlot()
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dchisq" else "pchisq"
        min <- round(qchisq(.0005, df=df), 3)
        max <- round(qchisq(.9995, df=df), 3)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("seq(", min, ", ", max, ", length=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df=", df, 
            '), xlab=expression(chi^2), ylab="', fun, 
            '", main="Chi-Squared Distribution: df = ', df, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dchisq)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(dfEntry)
    tkwait.window(top)
    }

FDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "F Distribution")
    df1Var <- tclVar("")
    df2Var <- tclVar("")
    df1Entry <- tkentry(top, width="6", textvariable=df1Var)
    df2Entry <- tkentry(top, width="6", textvariable=df2Var)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
        if (is.na(df1)) {
            tkmessageBox(message="Numerator degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FDistributionPlot()
            return()
            }
        if (is.na(df2)) {
            tkmessageBox(message="Denominator degrees of freedom not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            FDistributionPlot()
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "df" else "pf"
        min <- round(qf(.0005, df1=df1, df2=df2), 3)
        max <- round(qf(.9995, df1=df1, df2=df2), 3)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("seq(", min, ", ", max, ", length=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df1=", df1, ", df2=", df2,
            '), xlab="f", ylab="', fun, 
            '", main="F Distribution: Numerator df = ', df1, ', Denominator df = ', df2, 
            '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(df)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Numerator degrees of freedom"), df1Entry, sticky="e")
    tkgrid(tklabel(top, text="Denominator degrees of freedom"), df2Entry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(df1Entry)
    tkwait.window(top)
    }

binomialDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Binomial Distribution")
    trialsVar <- tclVar("")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    functionVar <- tclVar("Probability")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        trials <- as.numeric(tclvalue(trialsVar))
        if (is.na(trials)) {
            tkmessageBox(message="Binomial trials not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialDistributionPlot()
            return()
            }
        prob <- as.numeric(tclvalue(probVar))
        if (is.na(prob)) {
            tkmessageBox(message="Probability of success not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            binomialDistributionPlot()
            return()
            }
        fun <- tclvalue(functionVar)
        min <- qbinom(.0005, size=trials, prob=prob)
        max <- qbinom(.9995, size=trials, prob=prob)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste(min, ":", max, sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        if (fun == "Probability"){
            doItAndPrint(paste("plot(.x, dbinom(.x, size=", trials, ", prob=", prob,
                '), xlab="Number of Successes", ylab="Probability Mass", main="Binomial Distribution: Trials = ', 
                trials, ', Probability of success = ', prob, '", type="h")', sep=""))
            doItAndPrint(paste("points(.x, dbinom(.x, size=", trials, ", prob=", prob,
                '), pch=16)', sep=""))
            }
        else {
            command <- "rep(.x, rep(2, length(.x)))"
            logger(paste(".x <- ", command, sep=""))
            assign(".x", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(paste("plot(.x[-1], pbinom(.x, size=", trials, ", prob=", prob,
                ')[-length(.x)], xlab="Number of Successes", ylab="Cumulative Probability", main="Binomial Distribution: Trials = ', 
                trials, ', Probability of success = ', prob, '", type="l")', sep=""))
            }
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dbinom)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot probability mass function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
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

PoissonDistributionPlot <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Poisson Distribution")
    meanVar <- tclVar("")
    meanEntry <- tkentry(top, width="6", textvariable=meanVar)
    functionVar <- tclVar("Probability")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        mean <- as.numeric(tclvalue(meanVar))
        if (is.na(mean)) {
            tkmessageBox(message="Mean not specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            PoissonDistributionPlot()
            return()
            }
        fun <- tclvalue(functionVar)
        min <- qpois(.0005, lambda=mean)
        max <- qpois(.9995, lambda=mean)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste(min, ":", max, sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        if (fun == "Probability"){
            doItAndPrint(paste("plot(.x, dpois(.x, lambda=", mean,
                '), xlab="x", ylab="Probability Mass", main="Poisson Distribution: Mean = ', 
                mean, '", type="h")', sep=""))
            doItAndPrint(paste("points(.x, dpois(.x, lambda=", mean,
                '), pch=16)', sep=""))
            }
        else {
            command <- "rep(.x, rep(2, length(.x)))"
            logger(paste(".x <- ", command, sep=""))
            assign(".x", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(paste("plot(.x[-1], ppois(.x, lambda=", mean,
                ')[-length(.x)], xlab="x", ylab="Probability Mass", main="Poisson Distribution: Mean = ', 
                mean, '", type="l")', sep=""))
            }
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dpois)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="mean"), meanEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot probability mass function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(meanEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
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
