# Distributions  -> Plot Distributions menu dialogs

# last modified 1 July 05 by J. Fox

normalDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Normal Distribution"))
    muVar <- tclVar("0")
    muEntry <- tkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dnorm" else "pnorm"
        min <- round(qnorm(.0005, mean=mu, sd=sigma), 3)
        max <- round(qnorm(.9995, mean=mu, sd=sigma), 3)
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
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dnorm")
    tkgrid(tklabel(top, text=gettextRcmdr("mu (mean)")), muEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("sigma (standard deviation)")), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=muEntry)
    }

tDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("t Distribution"))
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            errorCondition(recall=tDistributionPlot, message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dt" else "pt"
        min <- round(qt(.0005, df=df), 3)
        max <- round(qt(.9995, df=df), 3)
        command <- paste("seq(", min, ", ", max, ", length=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df=", df, 
            '), xlab="t", ylab="', fun, 
            '", main="t Distribution: df = ', df, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dt")
    tkgrid(tklabel(top, text=gettextRcmdr("Degrees of freedom")), dfEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=dfEntry)
    }

chisquareDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Chi-squared Distribution"))
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            errorCondition(recall=chisquareDistributionPlot,message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dchisq" else "pchisq"
        min <- round(qchisq(.0005, df=df), 3)
        max <- round(qchisq(.9995, df=df), 3)
        command <- paste("seq(", min, ", ", max, ", length=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df=", df, 
            '), xlab=expression(chi^2), ylab="', fun, 
            '", main="Chi-Squared Distribution: df = ', df, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dchisq")
    tkgrid(tklabel(top, text=gettextRcmdr("Degrees of freedom")), dfEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=dfEntry)
    }

FDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("F Distribution"))
    df1Var <- tclVar("")
    df2Var <- tclVar("")
    df1Entry <- tkentry(top, width="6", textvariable=df1Var)
    df2Entry <- tkentry(top, width="6", textvariable=df2Var)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
        if (is.na(df1)) {
            errorCondition(recall=FDistributionPlot, message=gettextRcmdr("Numerator degrees of freedom not specified."))
            return()
            }
        if (is.na(df2)) {
             errorCondition(recall=FDistributionPlot, message=gettextRcmdr("Denominator degrees of freedom not specified."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "df" else "pf"
        min <- round(qf(.0005, df1=df1, df2=df2), 3)
        max <- round(qf(.9995, df1=df1, df2=df2), 3)
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
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="df")
    tkgrid(tklabel(top, text=gettextRcmdr("Numerator degrees of freedom")), df1Entry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Denominator degrees of freedom")), df2Entry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=df1Entry)
    }

binomialDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Binomial Distribution"))
    trialsVar <- tclVar("")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    functionVar <- tclVar("Probability")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
        trials <- as.numeric(tclvalue(trialsVar))
        if (is.na(trials)) {
            errorCondition(recall=binomialDistributionPlot, message=gettextRcmdr("Binomial trials not specified."))
            return()
            }
        prob <- as.numeric(tclvalue(probVar))
        if (is.na(prob)) {
            errorCondition(recall=binomialDistributionPlot, message=gettextRcmdr("Probability of success not specified."))
            return()
            }
        fun <- tclvalue(functionVar)
        min <- qbinom(.0005, size=trials, prob=prob)
        max <- qbinom(.9995, size=trials, prob=prob)
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
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dbinom")
    tkgrid(tklabel(top, text=gettextRcmdr("Binomial trials")), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot probability mass function")), densityButton, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=trialsEntry)
    }

PoissonDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Poisson Distribution"))
    meanVar <- tclVar("")
    meanEntry <- tkentry(top, width="6", textvariable=meanVar)
    functionVar <- tclVar("Probability")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
        mean <- as.numeric(tclvalue(meanVar))
        if (is.na(mean)) {
            errorCondition(recall=PoissonDistributionPlot, message=gettextRcmdr("Mean not specified."))
            return()
            }
        fun <- tclvalue(functionVar)
        min <- qpois(.0005, lambda=mean)
        max <- qpois(.9995, lambda=mean)
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
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dpois")
    tkgrid(tklabel(top, text=gettextRcmdr("mean")), meanEntry, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot probability mass function")), densityButton, sticky="e")
    tkgrid(tklabel(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(meanEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=meanEntry)
    }
