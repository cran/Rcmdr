# Distributions menu dialogs for selecting samples

# last modified 26 July 06 by J. Fox

normalDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Normal Distribution"))
    dsname <- tclVar(gettextRcmdr("NormalSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    muVar <- tclVar("0")
    muEntry <- tkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=normalDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=normalDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                normalDistributionSamples()
                return()
                }
            }
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(sigma) || sigma <= 0) {
            errorCondition(recall=normalDistributionSamples, message=gettextRcmdr("Standard deviation must be positive."))
            return()
            }
        if (is.na(n) || n <= 0) {
            errorCondition(recall=normalDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=normalDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rnorm(", samples, "*", n, ", mean=", mu, ", sd=", sigma, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rnorm")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("mu (mean)")), muEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("sigma (standard deviation)")), sigmaEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=muEntry)
    }

# --- t distribution

tDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from t Distribution"))
    dsname <- tclVar(gettextRcmdr("tSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=tDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=tDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                tDistributionSamples()
                return()
                }
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            errorCondition(recall=tDistributionSamples, 
                message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        if (df<=0) {
            errorCondition(recall=tDistributionSamples, 
                message=gettextRcmdr("Degrees of freedom must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=tDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=tDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rt(", samples, "*", n, ", df=", df, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rt")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Degrees of freedom")), dfEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=dfEntry)
    }

# ------- chisquare distribution

chisquareDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Chi-squared Distribution"))
    dsname <- tclVar(gettextRcmdr("ChisquareSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    dfVar <- tclVar("")
    dfEntry <- tkentry(top, width="6", textvariable=dfVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=chisquareDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=chisquareDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                chisquareDistributionSamples()
                return()
                }
            }
        df <- as.numeric(tclvalue(dfVar))
        if (is.na(df)) {
            errorCondition(recall=chisquareDistributionSamples, 
                message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        if (df<=0) {
            errorCondition(recall=chisquareDistributionSamples, 
                message=gettextRcmdr("Degrees of freedom must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=chisquareDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=chisquareDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rchisq(", samples, "*", n, ", df=", df, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rchisq")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Degrees of freedom")), dfEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=dfEntry)
    }
    
# ------ F-distribution

FDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from F Distribution"))
    dsname <- tclVar(gettextRcmdr("FSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    df1Var <- tclVar("")
    df2Var <- tclVar("")
    df1Entry <- tkentry(top, width="6", textvariable=df1Var)
    df2Entry <- tkentry(top, width="6", textvariable=df2Var)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=FDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=FDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                FDistributionSamples()
                return()
                }
            }
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
        if (is.na(df1)) {
            errorCondition(recall=FDistributionSamples, 
                message=gettextRcmdr("Numerator degrees of freedom not specified."))
            return()
            }
        if (is.na(df2)) {
             errorCondition(recall=FDistributionSamples, 
                message=gettextRcmdr("Denominator degrees of freedom not specified."))
            return()
            }
        if (df1 <= 0) {
            errorCondition(recall=FDistributionSamples, 
                message=gettextRcmdr("Numerator degrees of freedom must be positive."))
            return()
            }
        if (df2 <= 0) {
            errorCondition(recall=FDistributionSamples, 
                message=gettextRcmdr("Denominator degrees of freedom must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=FDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=FDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rf(", samples, "*", n, ", df1=", df1, ", df2=", df2, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rf")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Numerator degrees of freedom")), df1Entry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Denominator degrees of freedom")), df2Entry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=df1Entry)
    }

# ----- exponential distribution

exponentialDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Exponential Distribution"))
    dsname <- tclVar(gettextRcmdr("ExponentialSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    rateVar <- tclVar("1")
    rateEntry <- tkentry(top, width="6", textvariable=rateVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=tDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=tDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                tDistributionSamples()
                return()
                }
            }
        rate <- as.numeric(tclvalue(rateVar))
        if (is.na(rate) || rate <= 0) {
            errorCondition(recall=exponentialDistributionPlot, 
                message=gettextRcmdr("Rate must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=tDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=tDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rexp(", samples, "*", n, ", rate=", rate, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rexp")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Rate")), rateEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=rateEntry)
    }
    
# ----- uniform distribution

uniformDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Uniform Distribution"))
    dsname <- tclVar(gettextRcmdr("UniformSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    minVar <- tclVar("0")
    maxVar <- tclVar("1")
    minEntry <- tkentry(top, width="6", textvariable=minVar)
    maxEntry <- tkentry(top, width="6", textvariable=maxVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=uniformDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=uniformDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                uniformDistributionSamples()
                return()
                }
            }
        minValue <- as.numeric(tclvalue(minVar))
        maxValue <- as.numeric(tclvalue(maxVar))
        if (is.na(minValue) || is.na(maxValue) || minValue >= maxValue) {
            errorCondition(recall=uniformDistributionSamples, 
                message=gettextRcmdr("Lower limit must be less than upper limit."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=uniformDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=uniformDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(runif(", samples, "*", n, ", min=", minValue, ", max=", maxValue, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="runif")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Minimum")), minEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Maximum")), maxEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=minEntry)
    }
    
# ----- beta distribution

betaDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Beta Distribution"))
    dsname <- tclVar(gettextRcmdr("BetaSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    shape1Var <- tclVar("")
    shape1Entry <- tkentry(top, width="6", textvariable=shape1Var)
    shape2Var <- tclVar("")
    shape2Entry <- tkentry(top, width="6", textvariable=shape2Var)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=betaDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=betaDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                betaDistributionSamples()
                return()
                }
            }
        shape1 <- as.numeric(tclvalue(shape1Var))
        shape2 <- as.numeric(tclvalue(shape2Var))
        if (is.na(shape1) || is.na(shape2)) {
            errorCondition(recall=betaDistributionSamples, 
                message=gettextRcmdr("Shapes not specified."))
            return()
            }
        if (shape1 <= 0 || shape2 <= 0) {
            errorCondition(recall=betaDistributionSamples, 
                message=gettextRcmdr("Shapes must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=betaDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=betaDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rbeta(", samples, "*", n, ", shape1=", shape1, ", shape2=", shape2, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rbeta")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Shape 1")), shape1Entry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Shape 2")), shape2Entry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=shape1Entry)
    }

# ---- Cauchy distribution

CauchyDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Cauchy Distribution"))
    dsname <- tclVar(gettextRcmdr("CauchySamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    locationVar <- tclVar("0")
    locationEntry <- tkentry(top, width="6", textvariable=locationVar)
    sVar <- tclVar("1")
    sEntry <- tkentry(top, width="6", textvariable=sVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=CauchyDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=CauchyDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                CauchyDistributionSamples()
                return()
                }
            }
        location <- as.numeric(tclvalue(locationVar))
        s <- as.numeric(tclvalue(sVar))
        if (is.na(s) || s <= 0) {
            errorCondition(recall=CauchyDistributionSamples, 
                message=gettextRcmdr("Scale must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=CauchyDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=CauchyDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rcauchy(", samples, "*", n, ", location=", location, ", scale=", s, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rcauchy")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Location")), locationEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Scale")), sEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=locationEntry)
    }

# ---- logistic distribution

logisticDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Logistic Distribution"))
    dsname <- tclVar(gettextRcmdr("LogisticSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    locationVar <- tclVar("0")
    locationEntry <- tkentry(top, width="6", textvariable=locationVar)
    sVar <- tclVar("1")
    sEntry <- tkentry(top, width="6", textvariable=sVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=logisticDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=logisticDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                CauchyDistributionSamples()
                return()
                }
            }
        location <- as.numeric(tclvalue(locationVar))
        s <- as.numeric(tclvalue(sVar))
        if (is.na(s) || s <= 0) {
            errorCondition(recall=logisticDistributionSamples, 
                message=gettextRcmdr("Scale must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=logisticDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=logisticDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rlogis(", samples, "*", n, ", location=", location, ", scale=", s, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rlogis")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Location")), locationEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Scale")), sEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=locationEntry)
    }
    
# ---- lognormal distribution

lognormalDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Log-Normal Distribution"))
    dsname <- tclVar(gettextRcmdr("LogNormalSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    meanlogVar <- tclVar("0")
    meanlogEntry <- tkentry(top, width="6", textvariable=meanlogVar)
    sdlogVar <- tclVar("1")
    sdlogEntry <- tkentry(top, width="6", textvariable=sdlogVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=lognormalDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=lognormalDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                normalDistributionSamples()
                return()
                }
            }
        meanlog <- as.numeric(tclvalue(meanlogVar))
        sdlog <- as.numeric(tclvalue(sdlogVar))
        if (is.na(sdlog) || sdlog <= 0) {
            errorCondition(recall=lognormalDistributionSamples, 
                message=gettextRcmdr("Standard deviation must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=lognormalDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=lognormalDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rlnorm(", samples, "*", n, ", meanlog=", meanlog, ", sdlog=", sdlog, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rlnorm")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Mean (log scale)")), meanlogEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Standard deviation (log scale)")), sdlogEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=meanlogEntry)
    }

# ---- gamma distribution

gammaDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Gamma Distribution"))
    dsname <- tclVar(gettextRcmdr("GammaSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    shapeVar <- tclVar("")
    shapeEntry <- tkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- tkentry(top, width="6", textvariable=sVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=gammaDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=gammaDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                gammaDistributionSamples()
                return()
                }
            }
        shape <- as.numeric(tclvalue(shapeVar))
        if (is.na(shape)) {
            errorCondition(recall=gammaDistributionSamples, 
                message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=gammaDistributionSamples, 
                message=gettextRcmdr("Shape must be positive."))
            return()
            }
        s <- as.numeric(tclvalue(sVar))
        if (is.na(s) || s <= 0) {
            errorCondition(recall=gammaDistributionSamples, 
                message=gettextRcmdr("Scale must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=gammaDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=gammaDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rgamma(", samples, "*", n, ", shape=", shape, ", scale=", s, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rgamma")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Shape")), shapeEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Scale (inverse rate)")), sEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=shapeEntry)
    }
    
# ---- Weibull distribution

WeibullDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Weibull Distribution"))
    dsname <- tclVar(gettextRcmdr("WeibullSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    shapeVar <- tclVar("")
    shapeEntry <- tkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- tkentry(top, width="6", textvariable=sVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=WeibullDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=WeibullDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                normalDistributionSamples()
                return()
                }
            }
        shape <- as.numeric(tclvalue(shapeVar))
        if (is.na(shape)) {
            errorCondition(recall=WeibullDistributionSamples, 
                message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=WeibullDistributionSamples, 
                message=gettextRcmdr("Shape must be positive."))
            return()
            }
        s <- as.numeric(tclvalue(sVar))
        if (is.na(s) || s <= 0) {
            errorCondition(recall=WeibullDistributionSamples, 
                message=gettextRcmdr("Scale must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=WeibullDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=WeibullDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rweibull(", samples, "*", n, ", shape=", shape, ", scale=", s, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rweibull")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Shape")), shapeEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Scale")), sEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=shapeEntry)
    }
    
# ---- Gumbel distribution

GumbelDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Gumbel Distribution"))
    dsname <- tclVar(gettextRcmdr("GumbelSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    shapeVar <- tclVar("")
    shapeEntry <- tkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- tkentry(top, width="6", textvariable=sVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=GumbelDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=GumbelDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                normalDistributionSamples()
                return()
                }
            }
        shape <- as.numeric(tclvalue(shapeVar))
        if (is.na(shape)) {
            errorCondition(recall=GumbelDistributionSamples, 
                message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=GumbelDistributionSamples, 
                message=gettextRcmdr("Shape must be positive."))
            return()
            }
        s <- as.numeric(tclvalue(sVar))
        if (is.na(s) || s <= 0) {
            errorCondition(recall=GumbelDistributionSamples, 
                message=gettextRcmdr("Scale must be positive."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=GumbelDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=GumbelDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(log(rweibull(", samples, "*", n, ", shape=", shape, ", scale=", s, ")), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rweibull")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Shape (log shape)")), shapeEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Scale (log scale)")), sEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=shapeEntry)
    }
    
# ---- binomial distribution

binomialDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Binomial Distribution"))
    dsname <- tclVar(gettextRcmdr("BinomialSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    trialsVar <- tclVar("1")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar) 
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=binomialDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=binomialDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                binomialDistributionSamples()
                return()
                }
            }
        prob <- as.numeric(tclvalue(probVar))
        if (is.na(prob)) {
            errorCondition(recall=binomialDistributionSamples, 
                message=gettextRcmdr("Probability of success not specified."))
            return()
            }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=binomialDistributionSamples, 
                message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        trials <- round(as.numeric(tclvalue(trialsVar)))
        if (is.na(trials)) {
            errorCondition(recall=binomialDistributionSamples, 
                message=gettextRcmdr("Binomial trials not specified."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=binomialDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=binomialDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rbinom(", samples, "*", n, ", size=", trials, ", prob=", prob, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rbinom")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Binomial trials")), trialsEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=trialsEntry)
    }
    
# ---- Poisson distribution

PoissonDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Poisson Distribution"))
    dsname <- tclVar(gettextRcmdr("PoissonSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    meanVar <- tclVar("")
    meanEntry <- tkentry(top, width="6", textvariable=meanVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=PoissonDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=PoissonDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                PoissonDistributionSamples()
                return()
                }
            }
        mean <- as.numeric(tclvalue(meanVar))
        if (is.na(mean)) {
            errorCondition(recall=PoissonDistributionPlot, 
                message=gettextRcmdr("Mean not specified."))
            return()
            }
        if (mean < 0) {
            errorCondition(recall=PoissonDistributionPlot, 
                message=gettextRcmdr("Poisson mean cannot be negative."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=PoissonDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=PoissonDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rpois(", samples, "*", n, ", lambda=", mean, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rpois")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Mean")), meanEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=meanEntry)
    }
    
# ---- geometric distribution

geomDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Geometric Distribution"))
    dsname <- tclVar(gettextRcmdr("GeometricSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"),
        initialValues=c("1", "0", "0"),
        labels=gettextRcmdr(c("Sample means", "Sample sums",
            "Sample standard deviations")))
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=geomDistributionSamples,
                message=gettextRcmdr("You must enter the name of a data set."))
            return()
            }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=geomDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                geomDistributionSamples()
                return()
                }
            }
        prob <- as.numeric(tclvalue(probVar))
        if (is.na(prob)) {
            errorCondition(recall=geomDistributionSamples, 
                message=gettextRcmdr("Probability of success not specified."))
            return()
            }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=geomDistributionSamples, 
                message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=geomDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=geomDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rgeom(", samples, "*", n, ", prob=", prob, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1)
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1)
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rgeom")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname,
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=probEntry)
    }


# ---- hypergeometric distribution

hyperDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Hypergeometric Distribution"))
    dsname <- tclVar(gettextRcmdr("HypergeometricSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="20", textvariable=dsname)
    mVar <- tclVar("1")
    mEntry <- tkentry(top, width="6", textvariable=mVar)
    nVar <- tclVar("1")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    kVar <- tclVar("1")
    kEntry <- tkentry(top, width="6", textvariable=kVar)
    sampleSizeVar <- tclVar("100")
    sampleSizeEntry <- tkentry(top, width="6", textvariable=sampleSizeVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"),
        initialValues=c("1", "0", "0"),
        labels=gettextRcmdr(c("Sample means", "Sample sums",
            "Sample standard deviations")))
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=geometricDistributionSamples,
                message=gettextRcmdr("You must enter the name of a data set."))
            return()
            }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=geometricDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                geometricDistributionSamples()
                return()
                }
            }
        m <- as.numeric(tclvalue(mVar))
        n <- as.numeric(tclvalue(nVar))
        k <- as.numeric(tclvalue(kVar))
        if ( is.na(m) ){
              errorCondition(recall=hyperDistributionSamples, 
                message=gettextRcmdr("The m parameter was not specified."))
              return()
        }
        if ( m < 0 ){
              errorCondition(recall=hyperDistributionSamples, 
                message=gettextRcmdr("The m parameter cannot be negative."))
              return()
        }
        m <- round(m)
        if ( is.na(n) ){
              errorCondition(recall=hyperDistributionSamples, 
                message=gettextRcmdr("The n parameter was not specified."))
              return()
        }
        if ( n < 0 ){
              errorCondition(recall=hyperDistributionSamples, 
                message=gettextRcmdr("The n parameter cannot be negative."))
              return()
        }
        n <- round(n)
        if ( is.na(k) ){
              errorCondition(recall=hyperDistributionSamples, 
                message=gettextRcmdr("The k parameter was not specified."))
              return()
        }
        k <- round(k)
        if ( k > (m + n) ){
                errorCondition(recall=hyperDistributionSamples,
                message=gettextRcmdr("The k parameter cannot be greater than m + n."))
                        return()
                    }
        if ( k < 0 ){
                errorCondition(recall=hyperDistributionSamples,
                message=gettextRcmdr("The k parameter cannot be negative."))
                        return()
                    }
        sampleSize <- as.numeric(tclvalue(sampleSizeVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(sampleSize) || sampleSize <= 0) {
            errorCondition(recall=geometricDistributionSamples, 
                message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=geometricDistributionSamples, 
                message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rhyper(", samples, "*", sampleSize, ", n=", n, ", m=", m, ", k=", k,"), ncol=", sampleSize, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1)
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (sampleSize == 1)
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', sampleSize,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", sampleSize, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", sampleSize, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", sampleSize, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rhyper")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname,
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("m (number of white balls in the urn)")), mEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("n (number of black balls in the urn)")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("k (number of balls drawn from the urn)")), kEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), sampleSizeEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=12, columns=2, focus=mEntry)
    }
    
negbinomialDistributionSamples <- function(){
    initializeDialog(title=gettextRcmdr("Sample from Negative Binomial Distribution"))
    dsname <- tclVar(gettextRcmdr("NegativeBinomialSamples"))
    dsFrame <- tkframe(top)
    entryDsname <- tkentry(dsFrame, width="22", textvariable=dsname)
    probVar <- tclVar(".5")
    probEntry <- tkentry(top, width="6", textvariable=probVar)
    trialsVar <- tclVar("1")
    trialsEntry <- tkentry(top, width="6", textvariable=trialsVar) 
    nVar <- tclVar("100")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
    samplesVar <- tclVar("1")
    samplesEntry <- tkentry(top, width="6", textvariable=samplesVar)
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
        initialValues=c("1", "0", "0"), 
        labels=gettextRcmdr(c("Sample means", "Sample sums", 
            "Sample standard deviations")))    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=negbinomialDistributionSamples, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=negbinomialDistributionSamples,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                negbinomialDistributionSamples()
                return()
                }
            }
        prob <- as.numeric(tclvalue(probVar))
        if (is.na(prob)) {
            errorCondition(recall=negbinomialDistributionSamples, 
              message=gettextRcmdr("Probability of success not specified."))
            return()
            }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=negbinomialDistributionSamples, 
              message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        trials <- round(as.numeric(tclvalue(trialsVar)))
        if (is.na(trials)) {
            errorCondition(recall=negbinomialDistributionSamples, 
              message=gettextRcmdr("Target number of successes not specified."))
            return()
            }
        if ( trials < 0){
              errorCondition(recall=negnegbinomialDistributionSamples, 
                message=gettextRcmdr("Target number of successes cannot be negative."))
              return()
          }
        n <- as.numeric(tclvalue(nVar))
        samples <- as.numeric(tclvalue(samplesVar))
        if (is.na(n) || n <= 0) {
            errorCondition(recall=negbinomialDistributionSamples, 
              message=gettextRcmdr("Sample size must be positive."))
            return()
            }
        if (is.na(samples) || samples <= 0) {
            errorCondition(recall=negbinomialDistributionSamples, 
              message=gettextRcmdr("Number of samples must be positive."))
            return()
            }
        command <- paste(dsnameValue, " <- as.data.frame(matrix(rnbinom(", samples, "*", n, ", size=", trials, ", prob=", prob, "), ncol=", n, "))", sep="")
        justDoIt(command)
        logger(command)
        command <- if (samples == 1) 
            paste("rownames(", dsnameValue, ') <- "sample"', sep="")
            else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        command <- if (n == 1) 
            paste("colnames(", dsnameValue, ') <- "obs"', sep="")
            else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', n,
                ', sep="")', sep="")
        justDoIt(command)
        logger(command)
        if (tclvalue(meanVariable) == "1") {
            command <- paste(dsnameValue, "$mean <- rowMeans(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sumVariable) == "1") {
            command <- paste(dsnameValue, "$sum <- rowSums(", dsnameValue,
                "[,1:", n, "])", sep="")
            justDoIt(command)
            logger(command)
            }
        if (tclvalue(sdVariable) == "1") {
            command <- paste(dsnameValue, "$sd <- apply(", dsnameValue,
                "[,1:", n, "], 1, sd)", sep="")
            justDoIt(command)
            logger(command)
            }
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="rnbinom")
    tkgrid(tklabel(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
        sticky="w")
    tkgrid(dsFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Target number of successes")), trialsEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Number of observations (columns) ")), nEntry, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(tklabel(top, text=gettextRcmdr("Add to Data Set:"), fg="blue"), sticky="w")
    tkgrid(checkBoxFrame, columnspan=2, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2, focus=trialsEntry)
    }

