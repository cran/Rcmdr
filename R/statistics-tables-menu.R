# Statistics Menu dialogs

# last modified 5 July 04 by J. Fox

    # Tables menu
    
twoWayTable <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkFactors(2)) return()
    initializeDialog(title="Two-Way Table")
    variablesFrame <- tkframe(top)
    rowBox <- variableListBox(variablesFrame, .factors, title="Row variable (pick one)")
    columnBox <- variableListBox(variablesFrame, .factors, title="Column variable (pick one)")
    subsetBox()
    onOK <- function(){
        row <- getSelection(rowBox)
        column <- getSelection(columnBox)
        if (length(row) == 0 || length(column) == 0){
            errorCondition(recall=twoWayTable, message="You must select two variables.")
            return()
            }
        if (row == column) {
            errorCondition(recall=twoWayTable, message="Row and column variables are the same.")
            return()
            }        
        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqTestVariable)
        expected <- tclvalue(expFreqVariable)
        fisher <- tclvalue(fisherTestVariable)
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
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
    OKCancelHelp(helpSubject="xtabs")
    radioButtons(name="percents", buttons=c("rowPercents", "columnPercents", "nonePercents"), 
        values=c("row", "column", "none"), initialValue="none", 
        labels=c("Row percentages", "Column percentages", "No percentages"), title="Compute Percentages")
    checkBoxes(frame="testsFrame", boxes=c("chisqTest", "expFreq", "fisherTest"), initialValues=c("1", "0", "0"),
        labels=c("Chisquare test of independence", "Print expected frequencies", "Fisher's exact test"))
    tkgrid(getFrame(rowBox), tklabel(variablesFrame, text="    "), getFrame(columnBox), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(tklabel(top, text="Hypothesis Tests", fg="blue"), sticky="w")
    tkgrid(testsFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
    }

multiWayTable <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkFactors(3)) return()
    initializeDialog(title="Multi-Way Table")
    variablesFrame <- tkframe(top)
    rowBox <- variableListBox(variablesFrame, .factors, title="Row variable (pick one)")
    columnBox <- variableListBox(variablesFrame, .factors, title="Column variable (pick one)")
    controlBox <- variableListBox(variablesFrame, .factors, selectmode="multiple", 
        title="Control variable(s) (pick one or more)")
    subsetBox()
    onOK <- function(){
        row <- getSelection(rowBox)
        column <- getSelection(columnBox)
        controls <- getSelection(controlBox)
        if (length(row) == 0 || length(column) == 0 || length(controls) == 0) {
            errorCondition(recall=multiWayTable, message="You must select row, column, and control variables")
            return()
            }
        if ((row == column) || is.element(row, controls) || is.element(column, controls)) {
            errorCondition(recall=multiWayTable, message="Row, column, and control variables must be different.")
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
    OKCancelHelp(helpSubject="xtabs")
    radioButtons(name="percents", buttons=c("rowPercents", "columnPercents", "nonePercents"), values=c("row", "column", "none"),
        initialValue="none", labels=c("Row percentages", "Column percentages", "No percentages"), title="Compute Percentages")
    tkgrid(getFrame(rowBox), tklabel(variablesFrame, text="    "), getFrame(columnBox), tklabel(variablesFrame, text="    "), 
        getFrame(controlBox), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

enterTable <- function(){
    env <- environment()
    initializeDialog(title="Enter and Analyze Two-Way Table")
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
            errorCondition(recall=enterTable, message=paste("Number of valid entries (", length(counts), ")\n",
                "not equal to number of rows (", nrows,") * number of columns (", ncols,").", sep=""))
            return()
            }
        if (length(unique(row.names)) != nrows){
            errorCondition(recall=enterTable, message="Row names are not unique.")
            return()
            }     
        if (length(unique(col.names)) != ncols){
            errorCondition(recall=enterTable, message="Column names are not unique.")
            return()
            }     
        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqVariable)
        expected <- tclvalue(expFreqVariable)
        fisher <- tclvalue(fisherVariable)
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
    OKCancelHelp(helpSubject="chisq.test")
    radioButtons(name="percents", buttons=c("rowPercents", "columnPercents", "nonePercents"), values=c("row", "column", "none"),
        initialValue="none", labels=c("Row percentages", "Column percentages", "No percentages"), title="Compute Percentages")
    checkBoxes(frame="testsFrame", boxes=c("chisq", "expFreq", "fisher"), initialValues=c("1", "0", "0"),
        labels=c("Chisquare test of independence", "Print expected frequencies", "Fisher's exact test"))
    tkgrid(tklabel(rowColFrame, text="Number of Rows:"), rowsSlider, rowsShow, sticky="w")
    tkgrid(tklabel(rowColFrame, text="Number of Columns:"), colsSlider, colsShow, sticky="w")
    tkgrid(rowColFrame, sticky="w")
    tkgrid(tklabel(top, text="Enter counts:", fg="blue"), sticky="w")
    tkgrid(outerTableFrame, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(tklabel(top, text="Hypothesis Tests", fg="blue"), sticky="w")
    tkgrid(testsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=7, columns=2)
    } 
