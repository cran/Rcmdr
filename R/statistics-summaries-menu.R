# Statistics Menu dialogs

# last modified 17 August 05 by J. Fox

    # Summaries menu
    
summarizeDataSet <- function(){
    nvar <- length(Variables())
    .activeDataSet <- ActiveDataSet()
    if (nvar > 10){
        response <- RcmdrTkmessageBox(message=sprintf(gettextRcmdr("There are %d variables in the data set %s.\nDo you want to proceed?"), nvar, .activeDataSet),
            icon="question", type="okcancel", default="cancel")
        if ("cancel" == tclvalue(response)) {
            tkfocus(CommanderWindow())
            return()
            }
        }
    doItAndPrint(paste("summary(", .activeDataSet, ")", sep=""))
    }

numericalSummaries <- function(){
##    if (!checkActiveDataSet()) return()
##    if (!checkNumeric()) return()
    initializeDialog(title=gettextRcmdr("Numerical Summaries"))
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sd"), initialValues=c("1", "1"), labels=gettextRcmdr(c("Mean", "Standard Deviation")))
    quantilesVariable <- tclVar("1")
    quantilesFrame <- tkframe(top)
    quantilesCheckBox <- tkcheckbutton(quantilesFrame, variable=quantilesVariable)
    quantiles <- tclVar("0,.25,.5,.75,1")
    quantilesEntry <- tkentry(quantilesFrame, width="20", textvariable=quantiles)
    groupsBox(recall=numericalSummaries, label=gettextRcmdr("Summarize by:"), initialLabel=gettextRcmdr("Summarize by groups"))
    onOK <- function(){
        x <- getSelection(xBox)
        if (length(x) == 0){
            errorCondition(recall=numericalSummaries, message=gettextRcmdr("You must select a variable."))
            return()
            }
        quants <- paste("c(", gsub(" ", ",", tclvalue(quantiles)), ")")
        .activeDataSet <- ActiveDataSet()
        var <- paste(.activeDataSet, "$", x, sep="")
        closeDialog()
        if (.groups == FALSE) {
            if (tclvalue(meanVariable) == "1") doItAndPrint(paste("mean(", var, ", na.rm=TRUE)", sep=""))
            if (tclvalue(sdVariable) == "1") doItAndPrint(paste("sd(", var, ", na.rm=TRUE)", sep=""))
            if (tclvalue(quantilesVariable) == "1") doItAndPrint(paste("quantile(", var, ", ",
                quants, ", na.rm=TRUE)", sep=""))
            }
        else {
            grps <- paste(.activeDataSet, "$", .groups, sep="")
            if (tclvalue(meanVariable) == "1") doItAndPrint(paste("by(", var, ", ", grps,
                ", mean, na.rm=TRUE)", sep=""))
            if (tclvalue(sdVariable) == "1") doItAndPrint(paste("by(", var, ", ", grps,
                ", sd, na.rm=TRUE)", sep=""))
            if (tclvalue(quantilesVariable) == "1") doItAndPrint(paste("by(", var, ", ", grps,
                ", quantile, na.rm=TRUE, probs=", quants,")", sep=""))
            }
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="quantile")
    tkgrid(getFrame(xBox), sticky="nw")    
    tkgrid(checkBoxFrame, sticky="w")
    tkgrid(tklabel(quantilesFrame, text=gettextRcmdr("Quantiles")), quantilesCheckBox,
        tklabel(quantilesFrame, text=gettextRcmdr(" quantiles:")), quantilesEntry, sticky="w")
    tkgrid(quantilesFrame, sticky="w")
    tkgrid(groupsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
    }

frequencyDistribution <- function(){
    initializeDialog(title=gettextRcmdr("Frequency Distribution"))
    xBox <- variableListBox(top, Factors(), title=gettextRcmdr("Variable (pick one)"))
    optionsFrame <- tkframe(top)
    goodnessOfFitVariable <- tclVar("0")
    goodnessOfFitCheckBox <- tkcheckbutton(optionsFrame, variable=goodnessOfFitVariable)
    onOK <- function(){
        x <- getSelection(xBox)
        if (length(x) == 0){
            errorCondition(recall=frequencyDistribution, message=gettextRcmdr("You must select a variable."))
            return()
            }
        goodnessOfFit <- tclvalue(goodnessOfFitVariable)
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        command <- paste("table(", .activeDataSet, "$", x, ")", sep="")
        logger(paste(".Table <-", command))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table  # counts")
        doItAndPrint("100*.Table/sum(.Table)  # percentages")
        env <- environment()
        if (goodnessOfFit == 1){
            initializeDialog(subwin, title=gettextRcmdr("Goodness-of-Fit Test"))
            hypothesisFrame <- tkframe(subwin)
            levs <- eval(parse(text=paste("levels(", .activeDataSet, "$", x, ")", sep="")))
            n.levs <- length(levs)
            assign(".entry.1", tclVar(paste("1/", n.levs, sep="")), envir=env)
            make.entries <- "tklabel(hypothesisFrame, text='Hypothesized probabilities:   ')"
            make.lev.names <- "tklabel(hypothesisFrame, text='Factor levels:')"
            for (i in 1:n.levs) {
                entry.varname <- paste(".entry.", i, sep="")
                assign(entry.varname, tclVar(paste("1/", n.levs, sep="")), envir=env)
                make.entries <- paste(make.entries, ", ", "tkentry(hypothesisFrame, width='5', textvariable=", 
                        entry.varname, ")", sep="")
                make.lev.names <- paste(make.lev.names, ", tklabel(hypothesisFrame, text='", levs[i], "')", sep="")
                }
            eval(parse(text=paste("tkgrid(", make.lev.names, ", sticky='w')", sep="")), envir=env)
            eval(parse(text=paste("tkgrid(", make.entries, ", stick='w')", sep="")), envir=env)
            tkgrid(hypothesisFrame, sticky="w")
            onOKsub <- function(){
                probs <- rep(NA, n.levs)
                for (i in 1:n.levs){
                    entry.varname <- paste(".entry.", i, sep="")
                    res <- try(
                        entry <- eval(parse(text=eval(parse(text=paste("tclvalue(", entry.varname,")", sep="")), envir=env))),
                        silent=TRUE)
                    if (class(res) == "try-error"){
                        errorCondition(subwin, message=gettextRcmdr("Invalid entry."))
                        return()
                        }
                    if (length(entry) == 0){
                        errorCondition(subwin, message=gettextRcmdr("Missing entry."))
                        return()
                        }
                    opts <- options(warn=-1)
                    probs[i] <- as.numeric(entry)
                    options(opts)
                    }
                probs <- na.omit(probs)
                if (length(probs) != n.levs){
                    errorCondition(subwin, message=sprintf(gettextRcmdr("Number of valid entries (%d)\nnot equal to number levels (%d)."), length(probs), 
                        n.levs))
                    return()
                    }
                if (any(probs < 0)){
                    errorCondition(subwin, message=gettextRcmdr("Negative probabilities not allowed."))
                    return()
                    }
                if (abs(sum(probs) - 1) > 0.001){
                    Message(message=gettextRcmdr("Probabilities rescaled to sum to 1."), type="warning")
                    probs <- probs/sum(probs)
                    }
                closeDialog(subwin)
                command <- paste("c(", paste(probs, collapse=","), ")", sep="")
                logger(paste(".Probs <-", command))
                assign(".Probs", justDoIt(command), envir=.GlobalEnv)
                doItAndPrint("chisq.test(.Table, p=.Probs)")
                logger("remove(.Probs)")
                remove(.Probs, envir=.GlobalEnv)
                }
            subOKCancelHelp(subwin)
            tkgrid(subButtonsFrame, sticky="w")
            dialogSuffix(subwin, rows=2, columns=1, onOK=onOKsub, focus=subwin)
            }            
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)  
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="table")
    tkgrid(getFrame(xBox), sticky="nw")    
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Chi-square goodness-of-fit test")), goodnessOfFitCheckBox, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=2)
    }

statisticsTable <- function(){
    initializeDialog(title=gettextRcmdr("Table of Statistics"))
    variablesFrame <- tkframe(top)
    groupBox <- variableListBox(variablesFrame, Factors(), selectmode="multiple", title=gettextRcmdr("Factors (pick one or more)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable (pick one)"))
    radioButtons(name="statistic", buttons=c("mean", "median", "sd"), labels=gettextRcmdr(c("Mean", "Median", "Standard deviation")), title=gettextRcmdr("Statistic"))
    otherVariable <- tclVar("")
    otherButton <- tkradiobutton(statisticFrame, variable=statisticVariable, value="other")
    otherEntry <- tkentry(statisticFrame, width="20", textvariable=otherVariable)   
    tkgrid(tklabel(statisticFrame, text=gettextRcmdr("Other (specify)")), otherButton, otherEntry, sticky="w")
    onOK <- function(){
        groups <- getSelection(groupBox)
        if (0 == length(groups)) {
            errorCondition(recall=statisticsTable, message=gettextRcmdr("No factors selected."))
            return()
            }
        response <- getSelection(responseBox)
        if (0 == length(response)) {
            errorCondition(recall=statisticsTable, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        statistic <- tclvalue(statisticVariable)
        if (statistic == "other") statistic <- tclvalue(otherVariable)
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        groups.list <- paste(paste(groups, "=", .activeDataSet, "$", groups, sep=""), collapse=", ")
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
             "), ", statistic, ", na.rm=TRUE)", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="tapply")
    tkgrid(getFrame(groupBox), tklabel(variablesFrame, text="    "),getFrame(responseBox), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(statisticFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1, focus=otherEntry)
    }
    
correlationMatrix <- function(){
    initializeDialog(title=gettextRcmdr("Correlation Matrix"))
    xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Variables (pick two or more)"))
    radioButtons(name="correlations", buttons=c("pearson", "spearman", "partial"), values=c("Pearson", "Spearman", "partial"),
        labels=gettextRcmdr(c("Pearson product-moment", "Spearman rank-order", "Partial")), title=gettextRcmdr("Type of Correlations"))
    onOK <- function(){
        correlations <- tclvalue(correlationsVariable)
        x <- getSelection(xBox)
        if (2 > length(x)) {
            errorCondition(recall=correlationMatrix, message=gettextRcmdr("Fewer than 2 variables selected."))
            return()
            }
        if ((correlations == "partial") && (3 > length(x))) {
            errorCondition(recall=correlationMatrix, message=gettextRcmdr("Fewer than 3 variables selected\nfor partial correlations."))
            return()
            }
        closeDialog()
        x <- paste('"', x, '"', sep="")
        .activeDataSet <- ActiveDataSet()
        if (correlations == "Pearson")
            doItAndPrint(paste("cor(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')], use="complete.obs")', sep=""))
        else if (correlations == "Spearman"){
            logger("# Spearman rank-order correlations")
            doItAndPrint(paste("cor(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')], use="complete.obs", method="spearman")', sep=""))
             }
        else doItAndPrint(paste("partial.cor(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')], use="complete.obs")', sep=""))    
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="cor")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(correlationsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1)
    }
