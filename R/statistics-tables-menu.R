# Statistics Menu dialogs

# last modified 5 June 04 by J. Fox

    # Tables menu
    
twoWayTable <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) < 2){
        tkmessageBox(message="There fewer than 2 factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Two-Way Table")
    rowFrame <- tkframe(top)
    columnFrame <- tkframe(top)
    rowBox <- tklistbox(rowFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    rowScroll <- tkscrollbar(rowFrame, repeatinterval=5, 
        command=function(...) tkyview(rowBox, ...))
    tkconfigure(rowBox, yscrollcommand=function(...) tkset(rowScroll, ...))
    for (var in .factors) tkinsert(rowBox, "end", var)
    columnBox <- tklistbox(columnFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    columnScroll <- tkscrollbar(columnFrame, repeatinterval=5, 
        command=function(...) tkyview(columnBox, ...))    
    tkconfigure(columnBox, yscrollcommand=function(...) tkset(columnScroll, ...))
    for (var in .factors) tkinsert(columnBox, "end", var)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    onOK <- function(){
        row <- as.character(tkget(rowBox, "active"))
        column <- as.character(tkget(columnBox, "active"))
        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqTest)
        expected <- tclvalue(expFreq)
        fisher <- tclvalue(fisherTest)
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        if (row == column) {
            tkmessageBox(message="Row and column variables are the same.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            twoWayTable()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("xtabs(~", row, "+", column, ", data=", .activeDataSet, 
            subset, ")", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        if (chisq == 1) {
            command <- "chisq.test(.Table, correct=FALSE)"
            logger(paste(".Test <- ", command, sep=""))
            assign(".Test", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(".Test")
            if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
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
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
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
    tkgrid(tklabel(percentsFrame, text="Compute Percentages", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Row percentages"), rowPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Column percentages"), columnPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="No percentages"), nonePercentsButton, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(tklabel(testsFrame, text="Hypothesis Tests", fg="blue"), sticky="w")
    tkgrid(tklabel(testsFrame, text="Chisquare test of independence"), chisqCheckBox, sticky="e")
    tkgrid(tklabel(testsFrame, text="Print expected frequencies"), expFreqCheckBox, sticky="e")
    tkgrid(tklabel(testsFrame, text="Fisher's exact test"), fisherCheckBox, sticky="e")
    tkgrid(testsFrame)
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(chisqCheckBox, sticky="w")
    tkgrid.configure(fisherCheckBox, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(rowScroll, sticky="ns")
    tkgrid.configure(columnScroll, sticky="ns")
    for (row in 0:6) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(rowBox, 0)
    tkselection.set(columnBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

multiWayTable <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) < 3){
        tkmessageBox(message="There fewer than 3 factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Multi-Way Table")
    rowFrame <- tkframe(top)
    columnFrame <- tkframe(top)
    controlFrame <- tkframe(top)
    rowBox <- tklistbox(rowFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    rowScroll <- tkscrollbar(rowFrame, repeatinterval=5, 
        command=function(...) tkyview(rowBox, ...))
    tkconfigure(rowBox, yscrollcommand=function(...) tkset(rowScroll, ...))
    for (var in .factors) tkinsert(rowBox, "end", var)
    columnBox <- tklistbox(columnFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    columnScroll <- tkscrollbar(columnFrame, repeatinterval=5, 
        command=function(...) tkyview(columnBox, ...))   
    tkconfigure(columnBox, yscrollcommand=function(...) tkset(columnScroll, ...))
    for (var in .factors) tkinsert(columnBox, "end", var)
    controlBox <- tklistbox(controlFrame, height=min(4, length(.factors)),
        selectmode="multiple", background="white", exportselection="FALSE")
    controlScroll <- tkscrollbar(controlFrame, repeatinterval=5, 
        command=function(...) tkyview(controlBox, ...))    
    tkconfigure(controlBox, yscrollcommand=function(...) tkset(controlScroll, ...))
    for (var in .factors) tkinsert(controlBox, "end", var)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
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
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            multiWayTable()
            return()
            }
        percents <- as.character(tclvalue(percentsVariable))
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("xtabs(~", row, "+", column, "+", paste(controls, collapse="+"),
            ", data=", .activeDataSet, subset, ")", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)                                             
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
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
    tkgrid(tklabel(percentsFrame, text="Compute Percentages", fg="blue"), columnspan=3, sticky="w")
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
    tkgrid.configure(rowScroll, sticky="ns")
    tkgrid.configure(columnScroll, sticky="ns")
    tkgrid.configure(controlScroll, sticky="ns")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:2) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(rowBox, 0)
    tkselection.set(columnBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

enterTable <- function(){
    env <- environment()
    top <- tktoplevel()
    tkwm.title(top, "Enter and Analyze Two-Way Table")
    outerTableFrame <- tkframe(top)
    assign(".tableFrame", tkframe(outerTableFrame), envir=env)
    setUpTable <- function(...){
        tkdestroy(get(".tableFrame", envir=env))
        assign(".tableFrame", tkframe(outerTableFrame), envir=env)
        nrows <- as.numeric(tclvalue(rowsValue))
        ncols <- as.numeric(tclvalue(colsValue))
        make.col.names <- "tklabel(.tableFrame, text='')"
        for (j in 1:ncols) {
            col.varname <- paste(".colname.", j, sep="")
            assign(col.varname, tclVar(j), envir=env)
            make.col.names <- paste(make.col.names, ", ", "tkentry(.tableFrame, width='5', textvariable=", 
                    col.varname, ")", sep="")
            }
        eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
        for (i in 1:nrows){   
            varname <- paste(".tab.", i, ".1", sep="") 
            assign(varname, tclVar("") , envir=env)
            row.varname <- paste(".rowname.", i, sep="")
            assign(row.varname, tclVar(i), envir=env)
            make.row <- paste("tkentry(.tableFrame, width='5', textvariable=",
                row.varname, ")", sep="")
            make.row <- paste(make.row, ", ", "tkentry(.tableFrame, width='5', textvariable=", 
                varname, ")", sep="")
            for (j in 2:ncols){
                varname <- paste(".tab.", i, ".", j, sep="")
                assign(varname, tclVar(""), envir=env)
                make.row <- paste(make.row, ", ", "tkentry(.tableFrame, width='5', textvariable=", 
                    varname, ")", sep="")
                }
            eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
            }
        tkgrid(get(".tableFrame", envir=env), sticky="w")
        }
    rowColFrame <- tkframe(top)
    rowsValue <- tclVar("2")
    rowsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=rowsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    rowsShow <- tklabel(rowColFrame, textvariable=rowsValue, width=2, justify="right")
    colsValue <- tclVar("2")
    colsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=colsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    colsShow <- tklabel(rowColFrame, textvariable=colsValue, width=2, justify="right")
    onOK <- function(){
        nrows <- as.numeric(tclvalue(rowsValue))
        ncols <- as.numeric(tclvalue(colsValue))
        cell <- 0
        counts <- rep(NA, nrows*ncols)
        row.names <- rep("", nrows)
        col.names <- rep("", ncols)
        for (i in 1:nrows) row.names[i] <- 
            eval(parse(text=paste("tclvalue(", paste(".rowname.", i, sep=""),")", sep="")))
        for (j in 1:ncols) col.names[j] <- 
            eval(parse(text=paste("tclvalue(", paste(".colname.", j, sep=""),")", sep="")))
        for (i in 1:nrows){
            for (j in 1:ncols){
                cell <- cell+1
                varname <- paste(".tab.", i, ".", j, sep="")
                counts[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
                }
            }
        counts <- na.omit(counts)
        if (length(counts) != nrows*ncols){
            tkmessageBox(message=paste("Number of valid entries (", length(counts), ")\n",
                "not equal to number of rows (", nrows,") * number of columns (", ncols,").", 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            enterTable()
            return()
            }
        if (length(unique(row.names)) != nrows){
            tkmessageBox(message="Row names are not unique.", icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            enterTable()
            return()
            }     
        if (length(unique(col.names)) != ncols){
            tkmessageBox(message="Column names are not unique.", icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            enterTable()
            return()
            }     
        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqTest)
        expected <- tclvalue(expFreq)
        fisher <- tclvalue(fisherTest)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("matrix(c(", paste(counts, collapse=","), "), ", nrows, ", ", ncols,
            ", byrow=TRUE)", sep="")
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".Table <- ", command, sep=""))
        command <- paste("c(",paste(paste("'", row.names, "'", sep=""), collapse=", "), ")", sep="")
        justDoIt(paste("rownames(.Table) <- ", command, sep=""))
        logger(paste("rownames(.Table) <- ", command, sep=""))
        command <- paste("c(",paste(paste("'", col.names, "'", sep=""), collapse=", "), ")", sep="")
        justDoIt(paste("colnames(.Table) <- ", command, sep=""))
        logger(paste("colnames(.Table) <- ", command, sep=""))
        doItAndPrint(".Table  # Counts")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        cat("\n")
        if (chisq == 1) {
            command <- "chisq.test(.Table, correct=FALSE)"
            logger(paste(".Test <- ", command, sep=""))
            assign(".Test", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(".Test")
            if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
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
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(chisq.test)
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
    tkgrid(tklabel(rowColFrame, text="Number of Rows:"), rowsSlider, rowsShow, sticky="w")
    tkgrid(tklabel(rowColFrame, text="Number of Columns:"), colsSlider, colsShow, sticky="w")
    tkgrid(rowColFrame, sticky="w")
    tkgrid(tklabel(top, text="Enter counts:", fg="blue"), sticky="w")
    tkgrid(outerTableFrame, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Compute Percentages", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Row percentages"), rowPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="Column percentages"), columnPercentsButton, sticky="w")
    tkgrid(tklabel(percentsFrame, text="No percentages"), nonePercentsButton, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(tklabel(testsFrame, text="Hypothesis Tests", fg="blue"), sticky="w")
    tkgrid(tklabel(testsFrame, text="Chisquare test of independence"), chisqCheckBox, sticky="e")
    tkgrid(tklabel(testsFrame, text="Print expected frequencies"), expFreqCheckBox, sticky="e")
    tkgrid(tklabel(testsFrame, text="Fisher's exact test"), fisherCheckBox, sticky="e")
    tkgrid(testsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(chisqCheckBox, sticky="w")
    tkgrid.configure(fisherCheckBox, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)        
    } 
