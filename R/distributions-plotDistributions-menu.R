# Distributions  -> Plot Distributions menu dialogs

# last modified 25 June 04 by J. Fox

normalDistributionPlot <- function(){
    initializeDialog(title="Normal Distribution")
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
    OKCancelHelp(helpSubject="dnorm")
    tkgrid(tklabel(top, text="mu (mean)"), muEntry, sticky="e")
    tkgrid(tklabel(top, text="sigma (standard deviation)"), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=muEntry)
    }

tDistributionPlot <- function(){
    initializeDialog(title="t Distribution")
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            errorCondition(recall=tDistributionPlot, message="Degrees of freedom not specified.")
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
    OKCancelHelp(helpSubject="dt")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=dfEntry)
    }

chisquareDistributionPlot <- function(){
    initializeDialog(title="Chi-squared Distribution")
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    functionVar <- tclVar("Density")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            errorCondition(recall=chisquareDistributionPlot,message="Degrees of freedom not specified.")
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
    OKCancelHelp(helpSubject="dchisq")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=dfEntry)
    }

FDistributionPlot <- function(){
    initializeDialog(title="F Distribution")
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
            errorCondition(recall=FDistributionPlot, message="Numerator degrees of freedom not specified.")
            return()
            }
        if (is.na(df2)) {
             errorCondition(recall=FDistributionPlot, message="Denominator degrees of freedom not specified.")
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
    OKCancelHelp(helpSubject="df")
    tkgrid(tklabel(top, text="Numerator degrees of freedom"), df1Entry, sticky="e")
    tkgrid(tklabel(top, text="Denominator degrees of freedom"), df2Entry, sticky="e")
    tkgrid(tklabel(top, text="Plot density function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=df1Entry)
    }

binomialDistributionPlot <- function(){
    initializeDialog(title="Binomial Distribution")
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
            errorCondition(recall=binomialDistributionPlot, message="Binomial trials not specified.")
            return()
            }
        prob <- as.numeric(tclvalue(probVar))
        if (is.na(prob)) {
            errorCondition(recall=binomialDistributionPlot, message="Probability of success not specified.")
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
    OKCancelHelp(helpSubject="dbinom")
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot probability mass function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=trialsEntry)
    }

PoissonDistributionPlot <- function(){
    initializeDialog(title="Poisson Distribution")
    meanVar <- tclVar("")
    meanEntry <- tkentry(top, width="6", textvariable=meanVar)
    functionVar <- tclVar("Probability")
    densityButton <- tkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- tkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        mean <- as.numeric(tclvalue(meanVar))
        if (is.na(mean)) {
            errorCondition(recall=PoissonDistributionPlot, message="Mean not specified.")
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
    OKCancelHelp(helpSubject="dpois")
    tkgrid(tklabel(top, text="mean"), meanEntry, sticky="e")
    tkgrid(tklabel(top, text="Plot probability mass function"), densityButton, sticky="e")
    tkgrid(tklabel(top, text="Plot distribution function"), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(meanEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=meanEntry)
    }
