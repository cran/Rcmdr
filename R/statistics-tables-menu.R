# Statistics Menu dialogs

# last modified 20 May 03 by J. Fox

    # Tables menu
    
twoWayTable <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Two-Way Table")
    rowFrame <- tkframe(top)
    columnFrame <- tkframe(top)
    rowScroll <- tkscrollbar(rowFrame, repeatinterval=5, 
        command=function(...) tkyview(rowBox, ...))
    columnScroll <- tkscrollbar(columnFrame, repeatinterval=5, 
        command=function(...) tkyview(columnBox, ...))    
    rowBox <- tklistbox(rowFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(rowScroll, ...))
    for (var in .factors) tkinsert(rowBox, "end", var)
    columnBox <- tklistbox(columnFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(columnScroll, ...))
    for (var in .factors) tkinsert(columnBox, "end", var)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable,
        xscrollcommand=function(...) tkset(subsetScroll, ...))
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkyview(subsetEntry, ...))
    onOK <- function(){
        row <- as.character(tkget(rowBox, "active"))
        column <- as.character(tkget(columnBox, "active"))
        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqTest)
        expected <- tclvalue(expFreq)
        fisher <- tclvalue(fisherTest)
        subset <- tclvalue(subsetVariable)
        subset <- if (subset == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        if (row == column) {
            tkmessageBox(message="Row and column variables are the same.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            twoWayTable()
            return()
            }

        tkgrab.release(top)
        tkdestroy(top)
        command <- paste("xtabs(~", row, "+", column, ", data=", .activeDataSet, 
            subset, ")", sep="")
        logger(paste(".Table <- ", command, sep=""))
        logger(".Table")
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        print(.Table)
        cat("\n")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        cat("\n")
        if (chisq == 1) {
            command <- "chisq.test(.Table, correct=FALSE)"
            logger(paste(".Test <- ", command, sep=""))
            assign(".Test", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(".Test")
            if (expected == 1) doItAndPrint(".Test$expected")
            warnText <- NULL
            if (0 < (nlt1 <- sum(.Test$expected < 1))) warnText <- paste(nlt1,
                "expected frequencies are less than 1")
            if (0 < (nlt5 <- sum(.Test$expected < 1))) warnText <- paste(warnText, "\n", nlt5,
                " expected frequencies are less than 5", sep="")
            if (!is.null(warnText)) tkmessageBox(message=warnText,
                icon="warning", type="ok")
            logger("remove(.Test)") 
            remove(.Test, envir=.GlobalEnv) 
            }
        if (fisher == 1) doItAndPrint("fisher.test(.Table)")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)                                                      
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(xtabs)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    percentsVariable <- tclVar("none")
    percentsFrame <- tkframe(top)
    rowPercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="row")
    columnPercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="column")
    nonePercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="none")
    chisqTest <- tclVar("1")
    expFreq <- tclVar("0")
    fisherTest <- tclVar("0")
    testsFrame <- tkframe(top)
    chisqCheckBox <- tkcheckbutton(testsFrame, variable=chisqTest)
    expFreqCheckBox <- tkcheckbutton(testsFrame, variable=expFreq)
    fisherCheckBox <- tkcheckbutton(testsFrame, variable=fisherTest)
    tkgrid(tklabel(top, text="Row variable (pick one)"), 
        tklabel(top, text="Column variable (pick one)"), sticky="w")
    tkgrid(rowBox, rowScroll, sticky="nw")
    tkgrid(columnBox, columnScroll, sticky="nw")
    tkgrid(rowFrame, columnFrame, sticky="nw")
    tkgrid(tklabel(percentsFrame, text="Compute Percentages"), columnspan=2, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Row percentages"), rowPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Column percentages"), columnPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="No percentages"), nonePercentsButton, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(tklabel(testsFrame, text="Chisquare test of independence"), chisqCheckBox, sticky="e")
    tkgrid(tklabel(testsFrame, text="Print expected frequencies"), expFreqCheckBox, sticky="e")
    tkgrid(tklabel(testsFrame, text="Fisher's exact test"), fisherCheckBox, sticky="e")
    tkgrid.configure(chisqCheckBox, sticky="w")
    tkgrid.configure(fisherCheckBox, sticky="w")
    tkgrid(testsFrame)
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(rowBox, 0)
    tkselection.set(columnBox, 0)
    tkgrid.configure(rowScroll, sticky="ns")
    tkgrid.configure(columnScroll, sticky="ns")
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

multiWayTable <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Multi-Way Table")
    rowFrame <- tkframe(top)
    columnFrame <- tkframe(top)
    controlFrame <- tkframe(top)
    rowScroll <- tkscrollbar(rowFrame, repeatinterval=5, 
        command=function(...) tkyview(rowBox, ...))
    columnScroll <- tkscrollbar(columnFrame, repeatinterval=5, 
        command=function(...) tkyview(columnBox, ...))   
    controlScroll <- tkscrollbar(controlFrame, repeatinterval=5, 
        command=function(...) tkyview(controlBox, ...))    
    rowBox <- tklistbox(rowFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(rowScroll, ...))
    for (var in .factors) tkinsert(rowBox, "end", var)
    columnBox <- tklistbox(columnFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(columnScroll, ...))
    for (var in .factors) tkinsert(columnBox, "end", var)
    controlBox <- tklistbox(controlFrame, height=min(4, length(.factors)),
        selectmode="multiple", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(controlScroll, ...))
    for (var in .factors) tkinsert(controlBox, "end", var)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable,
        xscrollcommand=function(...) tkset(subsetScroll, ...))
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkyview(subsetEntry, ...))
    onOK <- function(){
        row <- as.character(tkget(rowBox, "active"))
        column <- as.character(tkget(columnBox, "active"))
        controls <- .factors[as.numeric(tkcurselection(controlBox)) + 1]
        if (length(controls) == 0) {
            tkmessageBox(message="No control variable(s) specified",
                icon="error", type="ok")
            tkdestroy(top)
            multiWayTable()
            return()
            }
        if ((row == column) || is.element(row, controls) || is.element(column, controls)) {
            tkmessageBox(message="Row, column, and control variables must be different.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            multiWayTable()
            return()
            }

        percents <- as.character(tclvalue(percentsVariable))
        subset <- tclvalue(subsetVariable)
        subset <- if (subset == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        tkgrab.release(top)
        tkdestroy(top)
        command <- paste("xtabs(~", row, "+", column, "+", paste(controls, collapse="+"),
            ", data=", .activeDataSet, subset, ")", sep="")
        logger(paste(".Table <- ", command, sep=""))
        logger(".Table")
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        print(.Table)
        cat("\n")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)                                             
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(xtabs)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    percentsVariable <- tclVar("none")
    percentsFrame <- tkframe(top)
    rowPercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="row")
    columnPercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="column")
    nonePercentsButton <- tkradiobutton(percentsFrame, variable=percentsVariable, value="none")
    tkgrid(tklabel(top, text="Row variable (pick one)"), 
        tklabel(top, text="Column variable (pick one)"), 
        tklabel(top, text="Control variable(s) (pick one or more)"), sticky="w")
    tkgrid(rowBox, rowScroll, sticky="nw")
    tkgrid(columnBox, columnScroll, sticky="nw")
    tkgrid(controlBox, controlScroll, sticky="nw")
    tkgrid(rowFrame, columnFrame, controlFrame, sticky="nw")
    tkgrid(tklabel(percentsFrame, text="Compute Percentages"), columnspan=3, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Row percentages"), rowPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Column percentages"), columnPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="No percentages"), nonePercentsButton, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(rowBox, 0)
    tkselection.set(columnBox, 0)
    tkgrid.configure(rowScroll, sticky="ns")
    tkgrid.configure(columnScroll, sticky="ns")
    tkgrid.configure(controlScroll, sticky="ns")
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }
