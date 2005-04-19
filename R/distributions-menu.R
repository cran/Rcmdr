# Distributions menu dialogs

# last modified 2 April 05 by J. Fox

normalQuantiles <- function(){
    initializeDialog(title="Normal Quantiles")
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
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=normalQuantiles, message="No probabilities specified.")
            return()
            }
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qnorm(c(", quantiles, "), mean=", mu,
            ", sd=", sigma, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qnorm")
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="mu (mean)"), muEntry, sticky="e")
    tkgrid(tklabel(top, text="sigma (standard deviation)"), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }

normalProbabilities <- function(){
    initializeDialog(title="Normal Probabilities")
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
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=normalProbabilities, message="No values specified.")
            return()
            }
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pnorm(c(", probabilities, "), mean=", mu, 
            ", sd=", sigma, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pnorm")
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="mu (mean)"), muEntry, sticky="e")
    tkgrid(tklabel(top, text="sigma (standard deviation)"), sigmaEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }
    
tQuantiles <- function(){
    initializeDialog(title="t Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=tQuantiles, message="No probabilities specified.") 
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            errorCondition(recall=tQuantiles, message="Degrees of freedom not specified.")
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qt(c(", quantiles, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qt")
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=quantilesEntry)
    }
    
tProbabilities <- function(){
    initializeDialog(title="t Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        df <- as.numeric(tclvalue(dfVar))
        if ("" == probabilities) {
            errorCondition(recall=tProbabilities, message="No values specified.")
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            errorCondition(recall=tProbabilities, message="Degrees of freedom not specified.")
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pt(c(", probabilities, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pt")
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=probabilitiesEntry)
    }

chisqQuantiles <- function(){
    initializeDialog(title="Chi-Squared Quantiles")
    quantilesVar <- tclVar("")
    quantilesEntry <- tkentry(top, width="30", textvariable=quantilesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=chisqQuantiles, message="No probabilities specified.")
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            errorCondition(recall=chisqQuantiles, message="Degrees of freedom not specified.")
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qchisq(c(", quantiles, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qchisq")
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=quantilesEntry)
    }
    
chisqProbabilities <- function(){
    initializeDialog(title="Chi-Squared Probabilities")
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- tkentry(top, width="30", textvariable=probabilitiesVar)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- tkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- tkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=chisqProbabilities, message="No values specified.")
            return()
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            errorCondition(recall=chisqProbabilities, message="Degrees of freedom not specified.")
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pchisq(c(", probabilities, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pchisq")
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Degrees of freedom"), dfEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=probabilitiesEntry)
    }

FQuantiles <- function(){
    initializeDialog(title="F Quantiles")
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
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=FQuantiles, message="Probabilities not specified")
            return()
            }
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
        if (is.na(df1) || is.na(df2)) {
            errorCondition(recall=FQuantiles, message="Degrees of freedom not specified.")
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qf(c(", quantiles, "), df1=", df1, 
            ", df2=", df2, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qf")
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Numerator degrees of freedom"), df1Entry, sticky="e")
    tkgrid(tklabel(top, text="Denominator degrees of freedom"), df2Entry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }
    
FProbabilities <- function(){
    initializeDialog(title="F Probabilities")
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
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=FProbabilities, message="Values not specified.")
            return()
            }
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
        if (is.na(df1) || is.na(df2)) {
            errorCondition(recall=FProbabilities, message="Degrees of freedom not specified.")
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pf(c(", probabilities, "), df1=", df1, 
            ", df2=", df2, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pf")
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Numerator degrees of freedom"), df1Entry, sticky="e")
    tkgrid(tklabel(top, text="Denominator degrees of freedom"), df2Entry, sticky="e")    
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=probabilitiesEntry)
    }

binomialQuantiles <- function(){
    initializeDialog(title="Binomial Quantiles")
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
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        trials <- as.numeric(tclvalue(trialsVar))
        prob <- as.numeric(tclvalue(probVar))
        if ("" == quantiles) {
            errorCondition(recall=binomialQuantiles, message="Probabilities not specified.")
            return()
            }
        if (is.na(trials)) {
            errorCondition(recall=binomialQuantiles, message="Binomial trials not specified.")
            return()
            }
        if (is.na(prob)) {
            errorCondition(recall=binomialQuantiles, message="Probability of success not specified.")
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qbinom(c(", quantiles, "), size=", trials, 
            ", prob=", prob, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qbinom")
    tkgrid(tklabel(top, text="Probabilities"), quantilesEntry, sticky="e")
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(buttonsFrame,columnspan=2, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }
    
binomialProbabilities <- function(){
    initializeDialog(title="Cumulative Binomial Probabilities")
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
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        trials <- as.numeric(tclvalue(trialsVar))
        prob <- as.numeric(tclvalue(probVar))
        if ("" == probabilities) {
            errorCondition(recall=binomialProbabilities, message="Values not specified.") 
            return()
            }
        if (is.na(trials)) {
            errorCondition(recall=binomialProbabilities, message="Binomial trials not specified.")
            return()
            }
        if (is.na(prob)) {
            errorCondition(recall=binomialProbabilities, message="Probability of success not specified.")
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pbinom(c(", probabilities, "), size=", trials, 
            ", prob=", prob, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pbinom")
    tkgrid(tklabel(top, text="Variable value(s)"), probabilitiesEntry, sticky="e")
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")    
    tkgrid(tklabel(top, text="Lower tail"), lowerTailButton, sticky="e")
    tkgrid(tklabel(top, text="Upper tail"), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=probabilitiesEntry)
    }

binomialMass <- function(){
    checkTrials <- function(trials){
        tkmessageBox(message=paste("Number of trials", trials, "is large.\nCreate long output?"),
            icon="warning", type="yesno", default="no")
        }
    initializeDialog(title="Binomial Probabilities")
    trialsVar <- tclVar("")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    onOK <- function(){
        closeDialog()
        trials <- as.numeric(tclvalue(trialsVar))
        if (is.na(trials)) {
            errorCondition(recall=binomialMass, message="Binomial trials not specified.")
            return()
            }
        if (trials > 50){
            if ("no" == tclvalue(checkTrials(trials))){
                if (getRcmdr("grab.focus")) tkgrab.release(top)
                tkdestroy(top)
                binomialMass()
                return()
                }
            }
        prob <- as.numeric(tclvalue(probVar))
        if (is.na(prob)) {
            errorCondition(recall=binomialMass, message="Probability of success not specified.")
            return()
            }
        command <- paste("data.frame(Pr=dbinom(0:", trials, ", size=", trials, 
            ", prob=", prob, "))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- 0:", trials, sep=""))
        justDoIt(paste("rownames(.Table) <- 0:", trials, sep=""))
        doItAndPrint(".Table")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)       
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dbinom")
    tkgrid(tklabel(top, text="Binomial trials"), trialsEntry, sticky="e")
    tkgrid(tklabel(top, text="Probability of success"), probEntry, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    dialogSuffix(rows=3, columns=2, focus=trialsEntry)
    }

PoissonMass <- function(){
    checkRange <- function(range){
        tkmessageBox(message=paste("Range of values over which to plot,", range, ", is large.\nCreate long output?"),
            icon="warning", type="yesno", default="no")
        }
    initializeDialog(title="Poisson Probabilities")
    meanVar <- tclVar("")
    meanEntry <- tkentry(top, width="6", textvariable=meanVar)
    onOK <- function(){
        closeDialog()
        mean <- as.numeric(tclvalue(meanVar))
        if (is.na(mean)) {
            errorCondition(recall=PoissonMass, message="Poisson mean not specified.")
            return()
            }
        min <- qpois(.00005, lambda=mean)
        max <- qpois(.99995, lambda=mean)
        range <- max - min
        if (range > 50){
            if ("no" == tclvalue(checkRange(range))){
                if (getRcmdr("grab.focus")) tkgrab.release(top)
                tkdestroy(top)
                PoissonMass()
                return()
                }
            }
        command <- paste("data.frame(Pr=round(dpois(", min, ":", max, ", lambda=", mean, "), 4))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- ", min, ":", max, sep=""))
        justDoIt(paste("rownames(.Table) <- ", min, ":", max, sep=""))
        doItAndPrint(".Table")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)       
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dpois")
    tkgrid(tklabel(top, text="Mean"), meanEntry, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(meanEntry, sticky="w")
    dialogSuffix(rows=2, columns=2, focus=meanEntry)
    }
