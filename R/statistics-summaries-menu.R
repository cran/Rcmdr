# Statistics Menu dialogs

# last modified 4 Feb 04 by J. Fox

    # Summaries menu
    
summarizeDataSet <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    nvar <- length(.variables)
    if (nvar > 10){
        response <- tkmessageBox(message=paste("There are ", nvar, " variables in the data set ",
            .activeDataSet, ".\nDo you want to proceed?", sep=""),
            icon="question", type="okcancel", default="cancel")
        if ("cancel" == tclvalue(response)) {
            tkfocus(.commander)        
            return()
            }
        }
    doItAndPrint(paste("summary(", .activeDataSet, ")", sep=""))
    }

numericalSummaries <- function(){
    env <- environment()
    .groupsLabel <- tclVar("Summarize by groups")
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Numerical Summaries")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    checkBoxFrame <- tkframe(top)
    meanVariable <- tclVar("1")
    meanCheckBox <- tkcheckbutton(checkBoxFrame, variable=meanVariable)
    sdVariable <- tclVar("1")
    sdCheckBox <- tkcheckbutton(checkBoxFrame, variable=sdVariable)
    quantilesVariable <- tclVar("1")
    quantilesFrame <- tkframe(top)
    quantilesCheckBox <- tkcheckbutton(quantilesFrame, variable=quantilesVariable)
    quantiles <- tclVar("0,.25,.5,.75,1")
    quantilesEntry <- tkentry(quantilesFrame, width="20", textvariable=quantiles)
    .groups <- FALSE
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        quants <- paste("c(", gsub(" ", ",", tclvalue(quantiles)), ")")
        var <- paste(.activeDataSet, "$", x, sep="")
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        if (.groups == FALSE) {
            if (tclvalue(meanVariable) == "1") doItAndPrint(paste("mean(", var, ", na.rm=TRUE)", sep=""))
            if (tclvalue(sdVariable) == "1") doItAndPrint(paste("sd(", var, ", na.rm=TRUE)", sep=""))
            if (tclvalue(quantilesVariable) == "1") doItAndPrint(paste("quantile(", var, ", ",
                quants, ", na.rm=TRUE)", sep=""))
            }
        else {
            grps <- paste(.activeDataSet, "$", .groups, sep="")
            if (tclvalue(meanVariable) == "1") doItAndPrint(paste("by(", var, ",", grps,
                ", mean, na.rm=TRUE)", sep=""))
            if (tclvalue(sdVariable) == "1") doItAndPrint(paste("by(", var, ",", grps,
                ", sd, na.rm=TRUE)", sep=""))
            if (tclvalue(quantilesVariable) == "1") doItAndPrint(paste("by(", var, ",", grps,
                ", quantile, na.rm=TRUE, probs=", quants,")", sep=""))
            }
        tkfocus(.commander)
        }
    onGroups <- function(){
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        groupsFrame <- tkframe(subdialog)
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE")
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        onOKsub <- function() {
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=env)
            tclvalue(.groupsLabel) <- paste("Summarize by:", groups)
            tkconfigure(groupsButton, fg="blue")
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        onCancelSub <- function() {
            assign(".groups", FALSE, envir=env)
            tclvalue(.groupsLabel) <- "Summarize by groups"
            tkconfigure(groupsButton, fg="black")
            if (.grab.focus) tkgrab.release(subdialog)  
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        subButtonFrame <- tkframe(subdialog)
        OKSubButton <- tkbutton(subButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subButtonFrame, text="Cancel", fg="red", width="12", command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Groups (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(subButtonFrame, sticky="w")
        for (row in 0:2) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkgrid.configure(groupsScroll, sticky="ns")
        tkselection.set(groupsBox, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkbind(groupsBox, "<Double-ButtonPress-1>", onOKsub)
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(quantile)
        }
    helpButton <- tkbutton(buttonFrame, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, textvariable=.groupsLabel, command=onGroups)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")    
    tkgrid(tklabel(checkBoxFrame, text="Mean"), meanCheckBox, sticky="w")
    tkgrid(tklabel(checkBoxFrame, text="Standard deviation"), sdCheckBox, sticky="w")
    tkgrid(checkBoxFrame, sticky="w")
    tkgrid(tklabel(quantilesFrame, text="Quantiles"), quantilesCheckBox,
        tklabel(quantilesFrame, text=" quantiles:"), quantilesEntry, sticky="w")
    tkgrid(quantilesFrame)
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

frequencyDistribution <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Frequency Distribution")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .factors) tkinsert(xBox, "end", x)
    optionsFrame <- tkframe(top)
    goodnessOfFitVariable <- tclVar("0")
    goodnessOfFitCheckBox <- tkcheckbutton(optionsFrame, variable=goodnessOfFitVariable)
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        goodnessOfFit <- tclvalue(goodnessOfFitVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("table(", .activeDataSet, "$", x, ")", sep="")
        logger(paste(".Table <-", command))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table  # counts")
        doItAndPrint("100*.Table/sum(.Table)  # percentages")        
        env <- environment()
        if (goodnessOfFit == 1){
            subwin <- tktoplevel()
            tkwm.title(subwin, "Goodness-of-Fit Test")
            hypothesisFrame <- tkframe(subwin)
            levs <- eval(parse(text=paste("levels(", .activeDataSet, "$", x, ")", sep="")))
            n.levs <- length(levs)
            assign(".entry.1", tclVar(paste("1/", n.levs, sep="")), envir=env)
            make.entries <- "tklabel(hypothesisFrame, text='Hypothesized probabilities', fg='blue')"
            make.lev.names <- "tklabel(hypothesisFrame, text='Factor levels', fg='blue')"
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
                    probs[i] <- eval(parse(text=eval(parse(text=paste("tclvalue(", entry.varname,")", sep="")), envir=env)))
                    }
                probs <- na.omit(probs)
                if (length(probs) != n.levs){
                    tkmessageBox(message=paste("Number of valid entries (", length(probs), ")\n",
                        "not equal to number levels (", n.levs,").", 
                        sep=""), icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(sub)
                    tkdestroy(subwin)
                    return()
                    }
                if (any(probs < 0)){
                    tkmessageBox(message="Negative probabilities not allowed.", icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subwin)
                    tkdestroy(subwin)
                    return()
                    }
                if (abs(sum(probs) - 1) > 0.001){
                    tkmessageBox(message="Probabilities rescaled to sum to 1.", icon="warning", type="ok")
                    probs <- probs/sum(probs)
                    }
                if (.grab.focus) tkgrab.release(subwin)
                tkdestroy(subwin)
                command <- paste("c(", paste(probs, collapse=","), ")", sep="")
                logger(paste(".Probs <-", command))
                assign(".Probs", justDoIt(command), envir=.GlobalEnv)
                doItAndPrint("chisq.test(.Table, p=.Probs)")
                logger("remove(.Probs)")
                remove(.Probs, envir=.GlobalEnv)
                }
            onCancelSub <- function() {
                if (.grab.focus) tkgrab.release(subwin)
                tkfocus(.commander)
                tkdestroy(subwin)  
                } 
            buttonFrameSub <- tkframe(subwin)
            OKbuttonSub <- tkbutton(buttonFrameSub, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
            cancelButtonSub <- tkbutton(buttonFrameSub, text="Cancel", fg="red", width="12",command=onCancelSub)
            tkgrid(OKbuttonSub, tklabel(buttonFrameSub, text="    "), cancelButtonSub, sticky="w")
            tkgrid(buttonFrameSub, sticky="w")
            for (row in 0:2) tkgrid.rowconfigure(subwin, row, weight=0)
            for (col in 0:0) tkgrid.columnconfigure(subwin, col, weight=0)
            .Tcl("update idletasks")
            tkwm.resizable(subwin, 0, 0)
            tkbind(subwin, "<Return>", onOKsub)
            if (.double.click) tkbind(subwin, "<Double-ButtonPress-1>", onOKsub)
            tkwm.deiconify(subwin)
            if (.grab.focus) tkgrab.set(subwin)
            tkfocus(subwin)
            tkwait.window(subwin)
            }            
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)  
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    buttonFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(table)
        }
    helpButton <- tkbutton(buttonFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")    
    tkgrid(tklabel(optionsFrame, text="Chi-square goodness-of-fit test"), goodnessOfFitCheckBox, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(xBox, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

statisticsTable <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) == 0){
        tkmessageBox(message="There no numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Table of Statistics")
    variablesFrame <- tkframe(top)
    groupFrame <- tkframe(variablesFrame)
    responseFrame <- tkframe(variablesFrame)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="multiple", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    statisticVariable <- tclVar("mean")
    otherVariable <- tclVar("")
    statisticFrame <- tkframe(top)
    meanButton <- tkradiobutton(statisticFrame, variable=statisticVariable, value="mean")
    medianButton <- tkradiobutton(statisticFrame, variable=statisticVariable, value="median")
    sdButton <- tkradiobutton(statisticFrame, variable=statisticVariable, value="sd")
    otherButton <- tkradiobutton(statisticFrame, variable=statisticVariable, value="other")
    otherEntry <- tkentry(statisticFrame, width="20", textvariable=otherVariable)   
    onOK <- function(){
        groups <- .factors[as.numeric(tkcurselection(groupBox)) + 1]
        if (0 == length(groups)) {
            tkmessageBox(message="No factors selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            statisticsTable()
            return()
            }
        response <- as.character(tkget(responseBox, "active"))
        statistic <- tclvalue(statisticVariable)
        if (statistic == "other") statistic <- tclvalue(otherVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        groups.list <- paste(paste(groups, "=", .activeDataSet, "$", groups, sep=""), collapse=", ")
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
             "), ", statistic, ", na.rm=TRUE)", sep=""))
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
        help(tapply)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(variablesFrame, text="Factors (pick one or more)"),
        tklabel(variablesFrame, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(variablesFrame)
    tkgrid(tklabel(statisticFrame, text="Statistic", fg="blue"), sticky="w")
    tkgrid(tklabel(statisticFrame, text="Mean"), meanButton, sticky="w")
    tkgrid(tklabel(statisticFrame, text="Median"), medianButton, sticky="w")
    tkgrid(tklabel(statisticFrame, text="Standard deviation"), sdButton, sticky="w")
    tkgrid(tklabel(statisticFrame, text="Other (specify)"), otherButton, otherEntry, sticky="w")
    tkgrid(statisticFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
    
correlationMatrix <- function(){
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
    tkwm.title(top, "Correlation Matrix")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    correlationsVariable <- tclVar("Pearson")
    correlationsFrame <- tkframe(top)
    pearsonButton <- tkradiobutton(correlationsFrame, variable=correlationsVariable, value="Pearson")
    spearmanButton <- tkradiobutton(correlationsFrame, variable=correlationsVariable, value="Spearman")
    partialButton <- tkradiobutton(correlationsFrame, variable=correlationsVariable, value="partial")
    onOK <- function(){
        correlations <- tclvalue(correlationsVariable)
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        if (2 > length(x)) {
            tkmessageBox(message="Fewer than 2 variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            correlationMatrix()
            return()
            }
        if ((correlations == "partial") && (3 > length(x))) {
            tkmessageBox(message="Fewer than 3 variables selected\nfor partial correlations.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            correlationMatrix()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        x <- paste('"', x, '"', sep="")
        if (correlations == "Pearson")
            doItAndPrint(paste("cor(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')], use="complete.obs")', sep=""))
        else if (correlations == "Spearman"){
            logger("# Spearman rank-order correlations")
             doItAndPrint(paste("cor(apply(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')], 2, rank), use="complete.obs")', sep="")) 
             }
        else doItAndPrint(paste("partial.cor(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')], use="complete.obs")', sep=""))    
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
        help(cor)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick two or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(tklabel(correlationsFrame, text="Type of Correlations", fg="blue"), sticky="w")
    tkgrid(tklabel(correlationsFrame, text="Pearson product-moment"), pearsonButton, sticky="w")
    tkgrid(tklabel(correlationsFrame, text="Spearman rank-order"), spearmanButton, sticky="w")
    tkgrid(tklabel(correlationsFrame, text="Partial"), partialButton, sticky="w")
    tkgrid(correlationsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
