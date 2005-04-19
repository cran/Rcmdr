# last modified 3 April 2005 by J. Fox

# Data menu dialogs

newDataSet <- function() {
    initializeDialog(title="New Data Set")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=newDataSet, 
                message="You must enter the name of a data set.")  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=newDataSet,
                message=paste('"', dsnameValue, '" is not a valid name.', sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, "Data set"))){
                newDataSet()
                return()
                }
            }
        command <- "edit(as.data.frame(NULL))"
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        logger(paste(dsnameValue, "<-", command))
        if (eval(parse(text=paste("nrow(", dsnameValue, ")"))) == 0){
            errorCondition(recall=newDataSet, message="empty data set.")
            return()
            }
        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="edit.data.frame")
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(entryDsname, sticky="w")
    dialogSuffix(rows=2, columns=2, focus=entryDsname)
    }

selectActiveDataSet <- function(){
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()
    if ((length(dataSets) == 1) && !is.null(.activeDataSet)) {
        Message(message="There is only one dataset in memory.",
                type="warning")
        tkfocus(CommanderWindow())
        return()
        }
    if (length(dataSets) == 0){
        Message(message="There are no data sets from which to choose.",
                type="error")
        tkfocus(CommanderWindow())
        return()
        }
    initializeDialog(title="Select Data Set")
    dataSetsBox <- variableListBox(top, dataSets, title="Data Sets (pick one)", 
        initialSelection=if (is.null(.activeDataSet)) NULL else which(.activeDataSet == dataSets) - 1)
    onOK <- function(){
        activeDataSet(getSelection(dataSetsBox))
        closeDialog()
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="attach")
    tkgrid(getFrame(dataSetsBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }
    
listDataSetsInPackages <- function() doItAndPrint("data()")

Recode <- function(){
    processRecode <- function(recode){
        parts <- strsplit(recode, "=")[[1]]
        if (length(grep(",", parts[1])) > 0) paste("c(", parts[1], ") = ", parts[2], sep="")
            else paste(parts, collapse="=")
        }
#    if (!checkActiveDataSet()) return(invisible())
    dataSet <- activeDataSet()
    initializeDialog(title="Recode Variable")
    variablesBox <- variableListBox(top, Variables(), title="Variable to recode (pick one)")
    variablesFrame <- tkframe(top)
    newVariableName <- tclVar("variable")
    newVariable <- tkentry(variablesFrame, width="20", textvariable=newVariableName)
    recodesFrame <- tkframe(top)
    recodes <- tktext(recodesFrame, bg="white", font=getRcmdr("logFont"),
        height="5", width="40", wrap="none")
    recodesXscroll <- tkscrollbar(recodesFrame, repeatinterval=5, orient="horizontal",
        command=function(...) tkxview(recodes, ...))
    recodesYscroll <- tkscrollbar(recodesFrame, repeatinterval=5,
        command=function(...) tkyview(recodes, ...))
    tkconfigure(recodes, xscrollcommand=function(...) tkset(recodesXscroll, ...))
    tkconfigure(recodes, yscrollcommand=function(...) tkset(recodesYscroll, ...))
    asFactorFrame <- tkframe(top)
    asFactorVariable <- tclVar("1")
    asFactorCheckBox <- tkcheckbutton(asFactorFrame, variable=asFactorVariable)
    onOK <- function(){
        variable <- getSelection(variablesBox)
        if (length(variable) == 0) {
            errorCondition(recall=Recode, message="You must select a variable.")
            return()
            }
        newVar <- trim.blanks(tclvalue(newVariableName))
        if (!is.valid.name(newVar)){
            errorCondition(recall=Recode,
                message=paste('"', newVar, '" is not a valid name.', sep=""))
            return()
            }
        asFactor <- tclvalue(asFactorVariable) == "1"
        recode.directives <- gsub("\n", "; ", tclvalue(tkget(recodes, "1.0", "end")))
        check.empty <- gsub(";", "", gsub(" ", "", recode.directives))
        closeDialog()
        if ("" == check.empty) {
            errorCondition(recall=Recode,
                message="No recode directives specified.")
            return()
            }
        if (0 != length(grep("'", recode.directives))) {
            errorCondition(recall=Recode,
                message='Use only double-quotes (" ") in recode directives')
            return()
            }
        if (is.element(newVar, Variables())) {
            if ("no" == tclvalue(checkReplace(newVar))){
                Recode()
                return()
                }
            }
        recode.directives <- strsplit(recode.directives, ";")[[1]]
        recode.directives <- paste(sapply(recode.directives, processRecode), collapse=";") 
        cmd <- paste("recode(", dataSet,"$",variable, ", '", recode.directives, 
            "', as.factor.result=", asFactor, ")", sep="")
        logger(paste(dataSet,"$",newVar, " <- ", cmd, sep=""))
        justDoIt(paste(dataSet,"$",newVar, " <- ", cmd, sep=""))
        activeDataSet(dataSet, flushModel=FALSE)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="Recode")    
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(tklabel(variablesFrame, text="New variable name"), sticky="w")
    tkgrid(newVariable, sticky="w")
    tkgrid(tklabel(recodesFrame, text="Enter recode directives", fg="blue"), sticky="w")
    tkgrid(recodes, recodesYscroll, sticky="nw")
    tkgrid(recodesXscroll)
    tkgrid(variablesFrame, recodesFrame, sticky="nw")
    tkgrid(tklabel(asFactorFrame, text="Make new variable a factor"), asFactorCheckBox, 
        sticky="w")
    tkgrid(asFactorFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(recodesXscroll, sticky="ew")
    tkgrid.configure(recodesYscroll, sticky="ns")
    dialogSuffix(rows=4, columns=2, bindReturn=FALSE)        
    }

Compute <- function(){
#    if (!checkActiveDataSet()) return(invisible())
    onDoubleClick <-function(){
        var <- trim.blanks(getSelection(variablesBox))
        if (length(grep("\\[factor\\]", var)) == 1)
            var <- trim.blanks(sub("\\[factor\\]", "",  var))
        tkfocus(compute)
        expr <- tclvalue(computeVar)
        tclvalue(computeVar) <- if (expr == "") var
            else paste(expr, var, sep=if (rev(strsplit(expr, "")[[1]])[1] =="(" ) "" else " ")
        tkicursor(compute, "end")
        tkxview.moveto(compute, "1")
        }
    dataSet <- activeDataSet()
    initializeDialog(title="Compute New Variable")
    .variables <- Variables()
    variables <- paste(.variables, ifelse(is.element(.variables, Factors()), "[factor]", ""))
    variablesBox <- variableListBox(top, variables, title="Current variables (double-click to expression)")
    tkbind(variablesBox$listbox, "<Double-ButtonPress-1>", onDoubleClick)
    variablesFrame <- tkframe(top)
    newVariableName <- tclVar("variable")
    newVariable <- tkentry(variablesFrame, width="20", textvariable=newVariableName)
    computeFrame <- tkframe(top)
    computeVar <- tclVar("")
    compute <- tkentry(computeFrame, font=getRcmdr("logFont"), width="30", textvariable=computeVar)
    computeXscroll <- tkscrollbar(computeFrame, repeatinterval=10,
        orient="horizontal", command=function(...) tkxview(compute, ...))
    tkconfigure(compute, xscrollcommand=function(...) tkset(computeXscroll, ...))
    onOK <- function(){
        closeDialog()
        newVar <- trim.blanks(tclvalue(newVariableName))
        if (!is.valid.name(newVar)){
            errorCondition(recall=Compute,
                message=paste('"', newVar, '" is not a valid name.', sep=""))
            return()
            }
        express <- tclvalue(computeVar)
        check.empty <- gsub(";", "", gsub(" ", "", express))
        if ("" == check.empty) {
            errorCondition(recall=Compute,
                message="No expression specified.")
            return()
            }
        if (is.element(newVar, Variables())) {
            if ("no" == tclvalue(checkReplace(newVar, "Variable"))){
                Compute()
                return()
                }
            }
        command <-  paste(dataSet,"$",newVar, " <- with(", ActiveDataSet(),
            ", ", express, ")", sep="")
        logger(command)
        justDoIt(command)
        activeDataSet(dataSet, flushModel=FALSE)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="Compute")
    tkgrid(getFrame(variablesBox), sticky="nw", columnspan=2)
    tkgrid(tklabel(variablesFrame, text="New variable name"), sticky="w")
    tkgrid(newVariable, tklabel(variablesFrame, text="     "), sticky="w")
    tkgrid(tklabel(computeFrame, text="Expression to compute"), sticky="w")
    tkgrid(compute, sticky="w")
    tkgrid(computeXscroll, sticky="ew")
    tkgrid(variablesFrame, computeFrame, sticky="nw")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    dialogSuffix(rows=3, columns=2, focus=compute)
    }

deleteVariable <- function(){
#    if (!checkActiveDataSet()) return(invisible())
    dataSet <- activeDataSet()
    initializeDialog(title="Delete Variables")
    variablesBox <- variableListBox(top, Variables(),
        title="Variable(s) to delete (pick one or more)", selectmode="multiple",
        initialSelection=NULL)
    onOK <- function(){
        variables <- getSelection(variablesBox)
        closeDialog()
        if (length(variables) == 0) {
            errorCondition(recall=deleteVariable, message="You must select one or more variables.")
            return()
            }
        if (length(variables) == 1){
            response <- tclvalue(tkmessageBox(message=paste("Delete ", variables,
                "?\nPlease confirm.", sep=""), icon="warning", type="okcancel", default="cancel"))
            if (response == "cancel") {
                onCancel()
                return()
                }
            }
        else{
            response <- tclvalue(tkmessageBox(message=paste("Delete ", length(variables),
                " variables?\nPlease confirm.", sep=""), 
                icon="warning", type="okcancel", default="cancel"))
            if (response == "cancel") {
                onCancel()
                return()
                }
            }  
        for (variable in variables){              
            eval(parse(text=paste(dataSet, "$", variable, "<- NULL", sep="")), envir=.GlobalEnv)
            logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
            }
        activeDataSet(dataSet, flushModel=FALSE)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="NULL")  
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }

readDataSet <- function() {
    initializeDialog(title="Read Data From Text File")
    optionsFrame <- tkframe(top)
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(optionsFrame, width="20", textvariable=dsname)
    headerVariable <- tclVar("1")
    headerCheckBox <- tkcheckbutton(optionsFrame, variable=headerVariable)
    radioButtons(optionsFrame, "delimiter", buttons=c("whitespace", "commas", "tabs"),
        labels=c("White space", "Commas", "Tabs"), title="Field Separator")
    otherButton <- tkradiobutton(delimiterFrame, variable=delimiterVariable, value="other")
    otherVariable <- tclVar("")
    otherEntry <- tkentry(delimiterFrame, width="4", textvariable=otherVariable) 
    radioButtons(optionsFrame, "decimal", buttons=c("period", "comma"),
        labels=c("Period [.]", "Comma [,]"), title="Decimal-Point Character")
    missingVariable <- tclVar("NA")
    missingEntry <- tkentry(optionsFrame, width="8", textvariable=missingVariable)    
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            errorCondition(recall=readDataSet,
                message="You must enter a name for the data set.")
                return()
                }
        if (!is.valid.name(dsnameValue)){
            errorCondition(recall=readDataSet,
                message=paste('"', dsnameValue, '" is not a valid name.', sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, "Data set"))){
                readDataSet()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(filetypes=
            '{"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}} {"All Files" {"*"}}'))
        if (file == "") {
            if (getRcmdr("grab.focus")) tkgrab.release(top)
            tkdestroy(top)
            return()
            }
        head <- tclvalue(headerVariable) == "1"
        delimiter <- tclvalue(delimiterVariable)
        del <- if (delimiter == "whitespace") ""
            else if (delimiter == "commas") ","
            else if (delimiter == "tabs") "\\t"
            else tclvalue(otherVariable)
        miss <- tclvalue(missingVariable)
        dec <- if (tclvalue(decimalVariable) == "period") "." else ","
        command <- paste('read.table("', file,'", header=', head, 
            ', sep="', del, '", na.strings="', miss, '", dec="', dec, '", strip.white=TRUE)', sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="read.table")
    tkgrid(tklabel(optionsFrame, text="Enter name for data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Variable names in file:"), headerCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Missing data indicator:"), missingEntry, sticky="w")
    tkgrid(tklabel(delimiterFrame, text="Other"), otherButton, 
        tklabel(delimiterFrame, text="  Specify:"), otherEntry, sticky="w")
    tkgrid(delimiterFrame, sticky="w", columnspan=2)
    tkgrid(decimalFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }
    
readDataFromPackage <- function() {
    env <- environment()
    initializeDialog(title="Read Data From Package")
    dsname <- tclVar("")
    package <- NULL
    enterFrame <- tkframe(top)
    entryDsname <- tkentry(enterFrame, width="20", textvariable=dsname)
    packages <- sort(.packages())
    packages <- packages[! packages %in% c("base", "stats")]
    packages <- packages[sapply(packages, function(package) nrow(data(package=package)$results) > 0)]
    packagesBox <- variableListBox(top, packages, title="Select package:")
    packageDatasetFrame <- tkframe(top)
    packageFrame <- tkframe(packageDatasetFrame)
    packageBox <- tklistbox(packageFrame, height="4", exportselection="FALSE",
        selectmode="single", background="white")
    packageScroll <- tkscrollbar(packageFrame, repeatinterval=5,
        command=function(...) tkyview(packageBox, ...))
    tkconfigure(packageBox, yscrollcommand=function(...) tkset(packageScroll, ...))
    for (p in packages) tkinsert(packageBox, "end", p)
    datasetFrame <- tkframe(packageDatasetFrame)
    datasetBox <- tklistbox(datasetFrame, height="4", exportselection="FALSE",
        selectmode="single", background="white")
    datasetScroll <- tkscrollbar(datasetFrame, repeatinterval=5,
        command=function(...) tkyview(datasetBox, ...))
    tkconfigure(datasetBox, yscrollcommand=function(...) tkset(datasetScroll, ...))
    onPackageSelect <- function(){
        assign("package", packages[as.numeric(tkcurselection(packageBox)) + 1], envir=env)
        datasets <- data(package=package)$results[,3]
        tkdelete(datasetBox, "0", "end")
        for (dataset in datasets) tkinsert(datasetBox, "end", dataset)
        tkconfigure(datasetBox, height=min(4, length(datasets)))
        }
    onDatasetSelect <- function(){
        tclvalue(dsname) <- data(package=package)$results[as.numeric(tkcurselection(datasetBox)) + 1,3]
        }
    onOK <- function(){
        datasetName <- data(package=package)$results[as.numeric(tkcurselection(datasetBox)) + 1,3]
        dsnameValue <- tclvalue(dsname)
        if (dsnameValue != "" && is.null(package)){
            closeDialog()
            if (is.element(dsnameValue, listDataSets())) {
                if ("no" == tclvalue(checkReplace(dsnameValue, "Data set"))){
                    if (GrabFocus()) tkgrab.release(top)
                    tkdestroy(top)
                    readDataFromPackage()
                    return()
                    }
                }
            save.options <- options(warn=2)
            check <- try(eval(parse(text=logger(paste("data(", dsnameValue, ")", sep=""))),
                envir=.GlobalEnv), silent=TRUE)
            options(save.options)
            if (class(check) == "try-error"){
                errorCondition(recall=readDataFromPackage,
                    message=paste("Data set", dsnameValue, "does not exist."))
                return()
                }
            activeDataSet(dsnameValue)
            tkfocus(CommanderWindow())
            }
        else{
            if (is.null(package)) {
                errorCondition(recall=readDataFromPackage, message="You must select a package.")
                return()
                }
            if (length(datasetName) == 0) {
                errorCondition(recall=readDataFromPackage, message="You must select a data set.")
                return()
                }
            if (is.element(datasetName, listDataSets())) {
                if ("no" == tclvalue(checkReplace(datasetName, "Data set"))){
                    if (GrabFocus()) tkgrab.release(top)
                    tkdestroy(top)
                    readDataFromPackage()
                    return()
                    }
                }
            closeDialog()
            command <- paste("data(", datasetName, ', package="', package, '")', sep="")
            justDoIt(command)
            logger(command)
            activeDataSet(datasetName)
            tkfocus(CommanderWindow())
            }
        }
    OKCancelHelp(helpSubject="data")
    tkgrid(tklabel(packageDatasetFrame, text="Package (Double-click to select)", fg="blue"),
    tklabel(packageDatasetFrame, text="   "), tklabel(packageDatasetFrame, text="Data set (Double-click to select)",
        fg="blue"), sticky="w")
    tkgrid(packageBox, packageScroll, sticky="nw")
    tkgrid(datasetBox, datasetScroll, sticky="nw")
    tkgrid(packageFrame, tklabel(packageDatasetFrame, text="   "), datasetFrame, sticky="nw")
    tkgrid(packageDatasetFrame, sticky="w")
    tkgrid(tklabel(top, text="OR", fg="red"), sticky="w")
    tkgrid(tklabel(enterFrame, text="Enter name of data set:  ", fg="blue"), entryDsname, sticky="w")
    tkgrid(enterFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(packageScroll, sticky="ns")
    tkgrid.configure(datasetScroll, sticky="ns")
    tkbind(packageBox, "<Double-ButtonPress-1>", onPackageSelect)
    tkbind(datasetBox, "<ButtonPress-1>", onDatasetSelect)
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    dialogSuffix(rows=4, columns=1, focus=entryDsname)
    }
    
importSPSS <- function() {
    initializeDialog(title="Import SPSS Data Set")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    asFactor <- tclVar("1")
    asFactorCheckBox <- tkcheckbutton(top, variable=asFactor)
    maxLevels <- tclVar("Inf")
    entryMaxLevels <- tkentry(top, width="5", textvariable=maxLevels)
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            errorCondition(recall=importSPSS,
                message="You must enter the name of a data set.")
                return()
                }
        if (!is.valid.name(dsnameValue)){
            errorCondition(recall=importSPSS,
                message=paste('"', dsnameValue, '" is not a valid name.', sep=""))
            return()
            }                     
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, "Data set"))){
                importSPSS()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(
            filetypes='{"SPSS save files" {".sav" ".SAV"}} {"SPSS portable files" {".por" ".POR"}} {"All Files" {"*"}}'))
        if (file == "") {
            tkfocus(CommanderWindow())
            return()
            }
        factor <- tclvalue(asFactor) == "1"
        levels <- as.numeric(tclvalue(maxLevels))
        command <- paste('read.spss("', file,'", use.value.labels=', factor,
            ", max.value.labels=", levels, ", to.data.frame=TRUE)", sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="read.spss")
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(top, text="Convert value labels\nto factor levels", justify="left"), 
        asFactorCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Maximum number\nof value labels\nfor factor conversion", justify="left"), 
        entryMaxLevels, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(entryDsname, sticky="w")
    tkgrid.configure(asFactorCheckBox, sticky="w")
    tkgrid.configure(entryMaxLevels, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=entryDsname)
    }

importMinitab <- function() {
    initializeDialog(title="Import Minitab Data Set")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            errorCondition(recall=importMinitab,
                message="You must enter the name of a data set.")
                return()
                }     
        if (!is.valid.name(dsnameValue)){
            errorCondition(recall=importMinitab,
                message=paste('"', dsnameValue, '" is not a valid name.', sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, "Data set"))){
                importMinitab()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(
            filetypes='{"Minitab portable files" {".mtp" ".MTP"}} {"All Files" {"*"}}'))
        if (file == "") {
            tkfocus(CommanderWindow())
            return()
            }
        command <- paste('read.mtp("', file,'")', sep="")
        datalist <- justDoIt(command)
        lengths <- sapply(datalist, length)
        datalist <- datalist[lengths != 0]
        lengths <- lengths[lengths != 0]
        if (!all(lengths == length(datalist[[1]]))){
            Message(message=
                paste("Minitab data set contains elements of unequal length.\nData set cannot be converted."),
                type="error")
            tkdestroy(top)
            tkfocus(CommanderWindow())
            return()
            }
        assign(dsnameValue, as.data.frame(datalist), envir=.GlobalEnv)
        logger(paste(dsnameValue, " <- as.data.frame(", command, ")", sep=""))
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="read.mtp")
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(entryDsname, sticky="w")
    dialogSuffix(rows=2, columns=2, focus=entryDsname)
    }

# the following function was contributed by Michael Ash (modified by J. Fox 2 Feb 05)

importSTATA <- function() {
    initializeDialog(title="Import STATA Data Set")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    asFactor <- tclVar("1")
    asFactorCheckBox <- tkcheckbutton(top, variable=asFactor)
    asDate <- tclVar("1")
    asDateCheckBox <- tkcheckbutton(top, variable=asDate)
    asMissingType <- tclVar("1")
    asMissingTypeCheckBox <- tkcheckbutton(top, variable=asMissingType)
    asConvertUnderscore <- tclVar("1")
    asConvertUnderscoreCheckBox <- tkcheckbutton(top, variable=asConvertUnderscore)
    asWarnMissingLabels <- tclVar("1")
    asWarnMissingLabelsCheckBox <- tkcheckbutton(top, variable=asWarnMissingLabels)
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            errorCondition(recall=importSTATA,
                message="You must enter the name of a data set.")
                return()
                }
        if (!is.valid.name(dsnameValue)){
            errorCondition(recall=importSTATA,
                message=paste('"', dsnameValue, '" is not a valid name.', sep=""))
            return()
            }                     
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, "Data set"))){
                importSTATA()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(
            filetypes='{"STATA datasets" {".dta" ".DTA"}} {"All Files" {"*"}}'))
        if (file == "") {
            tkfocus(CommanderWindow())
            return()
            }
        convert.date <- tclvalue(asDate) == "1"
        factor <- tclvalue(asFactor) == "1"
        missingtype <- tclvalue(asMissingType) == "1"
        convertunderscore <- tclvalue(asConvertUnderscore) == "1"
        warnmissinglabels <- tclvalue(asWarnMissingLabels) == "1"
        command <- paste('read.dta("', file,'", convert.dates=', convert.date,
            ", convert.factors=", factor, ", missing.type=", missingtype, 
            ", convert.underscore=", convertunderscore, ", warn.missing.labels=TRUE)", sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        activeDataSet(dsnameValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="read.dta")
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(top, text="Convert value labels\nto factor levels", justify="left"), 
        asFactorCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Convert dates to R format", justify="left"), 
        asDateCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Multiple missing types (>=Stata 8)", justify="left"), 
        asMissingTypeCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Convert underscore to period", justify="left"), 
        asConvertUnderscoreCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Warn on missing labels", justify="left"), 
        asWarnMissingLabelsCheckBox, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(entryDsname, sticky="w")
    tkgrid.configure(asFactorCheckBox, sticky="w")
    tkgrid.configure(asDateCheckBox, sticky="w")
    tkgrid.configure(asMissingTypeCheckBox, sticky="w")
    tkgrid.configure(asWarnMissingLabelsCheckBox, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=entryDsname)
    } 

numericToFactor <- function(){
#    if (!checkActiveDataSet()) return()
#    if (!checkNumeric()) return()
    initializeDialog(title="Convert Numeric Variable to Factor")
    variableBox <- variableListBox(top, Numeric(), title="Variable (pick one)")
    radioButtons(name="levels", buttons=c("names", "numbers"), 
        labels=c("Supply level names", "Use numbers"), title="Factor Levels")
    factorName <- tclVar("<same as variable>")
    factorNameField <- tkentry(top, width="20", textvariable=factorName)
    onOK <- function(){
        variable <- getSelection(variableBox)
        closeDialog()
        if (length(variable) == 0) {
            errorCondition(recall=numericToFactor, message="You must select a variable.")
            return()
            }
        name <- trim.blanks(tclvalue(factorName))
        if (name == "<same as variable>") name <- variable
        if (!is.valid.name(name)){
            errorCondition(recall=numericToFactor,
                message=paste('"', name, '" is not a valid name.', sep=""))
            return()
            }
        if (is.element(name, Variables())) {
            if ("no" == tclvalue(checkReplace(name))){
                numericToFactor()
                return()
                }
            }
        levelsType <- tclvalue(levelsVariable)
        if (levelsType == "names"){
            .activeDataSet <- ActiveDataSet()
            values <- sort(unique(eval(parse(text=paste(.activeDataSet, "$", variable, sep="")),
                envir=.GlobalEnv)))
            nvalues <- length(values)
            if (nvalues > 30) {
                errorCondition(recall=numericToFactor,
                    message=paste("Number of levels (", nvalues, ") too large.", sep=""))
                return()
                }
            initializeDialog(subdialog, title="Level Names")
            names <- rep("", nvalues)
            onOKsub <- function() {
                closeDialog(subdialog)
                for (i in 1:nvalues){
                    names[i] <- eval(parse(text=paste("tclvalue(levelName", i, ")", sep="")))
                    }
                if (length(unique(names)) != nvalues){
                    errorCondition(recall=numericToFactor,
                        message="Levels names are not unique.")
                    return()
                    }
                if (any(names == "")){
                    errorCondition(recall=numericToFactor,
                        message="A level name is empty.")
                    return()
                    }
                command <- paste("factor(", .activeDataSet, "$", variable,
                    ", labels=c(", paste(paste("'", names, "'", sep=""), collapse=","), "))", sep="")
                justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
                logger(paste(.activeDataSet,"$", name," <- ", command, sep=""))
                activeDataSet(.activeDataSet)
                tkfocus(CommanderWindow())
                }
            subOKCancelHelp()
            tkgrid(tklabel(subdialog, text="Numeric value"), tklabel(subdialog, text="Level name"), sticky="w")
            for (i in 1:nvalues){
                valVar <- paste("levelName", i, sep="")
                assign(valVar, tclVar(""))
                assign(paste("entry", i, sep=""), tkentry(subdialog, width="20", 
                    textvariable=eval(parse(text=valVar))))
                tkgrid(tklabel(subdialog, text=values[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
                }
            tkgrid(subButtonsFrame, sticky="w", columnspan=2)
            dialogSuffix(subdialog, rows=nvalues+2, columns=2, focus=entry1, onOK=onOKsub)   
            }
        else{
            command <- paste("as.factor(", .activeDataSet, "$", variable, ")", sep="")
            justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
            logger(paste(.activeDataSet, "$", name," <- ", command, sep=""))
            activeDataSet(.activeDataSet, flushModel=FALSE)
            tkfocus(CommanderWindow())
            }
        }
    OKCancelHelp(helpSubject="factor")
    tkgrid(getFrame(variableBox), levelsFrame, sticky="nw")
    tkgrid(tklabel(top, text="Name for factor"), sticky="w")
    tkgrid(factorNameField, sticky="w")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(numbersButton, sticky="w")
    tkgrid.configure(namesButton, sticky="w")
    dialogSuffix(rows=4, columns=2, preventGrabFocus=TRUE)
    }

binVariable <- function(){
# Author: Dan Putler (revision by J. Fox, 2 Feb 05)
#    if (!checkActiveDataSet()) return()
#    if (!checkNumeric()) return()
    env <- environment()
    initializeDialog(title="Bin a Numeric Variable")
    variableFrame <- tkframe(top)
    variableBox <- variableListBox(variableFrame, Numeric(), title="Variable to bin (pick one)")
    newVariableFrame <- tkframe(variableFrame)
    newVariableName <- tclVar("variable")
    newVariable <- tkentry(newVariableFrame, width="18", textvariable=newVariableName)
    binsFrame <- tkframe(top)
    binsVariable <- tclVar("3")
    slider <- tkscale(binsFrame, from=2, to=10, showvalue=TRUE, variable=binsVariable,
        resolution=1, orient="horizontal")
    optionsFrame <- tkframe(top)
    radioButtons(optionsFrame, name="levels", buttons=c("specify", "numbers", "ranges"),
        labels=c("Specify names", "Numbers", "Ranges"), title="Level Names")    
    radioButtons(optionsFrame, name="method", buttons=c("intervals", "proportions", "natural"),
        labels=c("Equal-width bins", "Equal-count bins", "Natural breaks\n(from K-means clustering)"),
        title="Binning Method")
    onOK <- function(){
        levels <- tclvalue(levelsVariable)
        bins <- as.numeric(tclvalue(binsVariable))
        varName <- getSelection(variableBox)
        closeDialog()
        if (length(varName) == 0){
            errorCondition(recall=binVariable, message="You must select a variable.")
            return()
            }
        newVar <- tclvalue(newVariableName)
        if (is.element(newVar, Variables())) {
                if ("no" == tclvalue(checkReplace(newVar))){
                    binVariable()
                    return()
                    }
                }
        if (!is.valid.name(newVar)){
            errorCondition(message=paste('"', newVar, '" is not a valid name.', sep=""),
                recall=binVariable)
            return()
            }
        method <- tclvalue(methodVariable)
        if (levels == "specify"){
            initializeDialog(subdialog, title="Bin Names")
            onOKsub <- function() {
                closeDialog(subdialog)
                level <- character(bins)
                for (i in 1:bins){
                    level[i] <- eval(parse(text=paste("tclvalue(levelName", i, ")", sep="")))
                    }
                if (length(unique(level)) != length(level)){
                    errorCondition(window=subdialog, message="Level names must be unique.",
                        recall=onOK)
                    return()
                    }
                assign("levelNames", level, envir=env)
                }
            subOKCancelHelp()
            tkgrid(tklabel(subdialog, text="Bin", fg="blue"), 
                tklabel(subdialog, text="Name", fg="blue"), sticky="w")
            for (i in 1:bins){
                valVar <- paste("levelName", i, sep="")
                assign(valVar, tclVar(i))
                assign(paste("entry", i, sep=""), tkentry(subdialog, width="20", 
                    textvariable=eval(parse(text=valVar))))
                tkgrid(tklabel(subdialog, text=as.character(i)), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
                }
            tkgrid(subButtonsFrame, sticky="w", columnspan=2)
            dialogSuffix(subdialog, focus=entry1, rows=bins+1, columns=2, bindReturn=FALSE)
            }
        labels <- if (levels == "numbers") "FALSE"
            else if (levels == "ranges") "NULL"
            else {
                if (!exists("levelNames")){
                    onCancel()
                    binVariable()
                    return()
                    }
                paste("c('", paste(levelNames,  collapse="','"), "')", sep="")
                }
        .activeDataSet <- ActiveDataSet()
        command <- paste(.activeDataSet,"$",newVar, " <- ",
            "bin.var(", .activeDataSet,"$", varName, ", bins=", bins,
            ", method=", "'", method, "', labels=", labels, ")", sep="")
        logger(command)
        justDoIt(command)
        activeDataSet(.activeDataSet, flushModel=FALSE)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="bin.var")
    tkgrid(tklabel(newVariableFrame, text="New variable name", fg="blue"), sticky="w") 
    tkgrid(newVariable, sticky="w")
    tkgrid(getFrame(variableBox), tklabel(variableFrame, text="    "), newVariableFrame, sticky="nw")
    tkgrid(variableFrame, sticky="w")
    tkgrid(tklabel(binsFrame, text="Number of bins:"), slider, sticky="s")
    tkgrid(binsFrame, sticky="w")
    tkgrid(levelsFrame, tklabel(optionsFrame, text="    "), methodFrame, sticky="nw")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

reorderFactor <- function(){
#    if (!checkActiveDataSet()) return()
#    if (!checkFactors()) return()
    initializeDialog(title="Reorder Factor Levels")
    variableBox <- variableListBox(top, Factors(), title="Factor (pick one)")
    orderedFrame <- tkframe(top)
    orderedVariable <- tclVar("0")
    orderedCheckBox <- tkcheckbutton(orderedFrame, variable=orderedVariable)
    factorName <- tclVar("<same as original>")
    factorNameField <- tkentry(top, width="20", textvariable=factorName)
    onOK <- function(){
        variable <- getSelection(variableBox)
        closeDialog()
        if (length(variable) == 0) {
            errorCondition(recall=reorderFactor, message="You must select a variable.")
            return()
            }
        name <- trim.blanks(tclvalue(factorName))
        if (name == "<same as original>") name <- variable
        if (!is.valid.name(name)){
            errorCondition(recall=reorderFactor,
                message=paste('"', name, '" is not a valid name.', sep=""))
            return()
            }
        if (is.element(name, Variables())) {
            if ("no" == tclvalue(checkReplace(name))){
                reorderFactor()
                return()
                }
            }
        .activeDataSet <- ActiveDataSet()
        old.levels <- eval(parse(text=paste("levels(", .activeDataSet, "$", variable, ")", 
            sep="")), envir=.GlobalEnv)
        nvalues <- length(old.levels)
        ordered <- tclvalue(orderedVariable)
        if (nvalues > 30) {
            errorCondition(recall=reorderFactor,
                message=paste("Number of levels (", nvalues, ") too large.", sep=""))
            return()
            }
        initializeDialog(subdialog, title="Reorder Levels")
        order <- 1:nvalues
        onOKsub <- function() {
            closeDialog(subdialog)
            opt <- options(warn=-1)
            for (i in 1:nvalues){
                order[i] <- as.numeric(eval(parse(text=paste("tclvalue(levelOrder", i, ")", sep=""))))
                }
            options(opt)
            if (any(sort(order) != 1:nvalues) || any(is.na(order))){
                errorCondition(recall=reorderFactor,
                    message=paste("Order of levels must include all integers from 1 to ", nvalues, sep=""))
                return()
                }
            levels <- old.levels[order(order)]
            ordered <- if (ordered == "1") ", ordered=TRUE" else ""
            command <- paste("factor(", .activeDataSet, "$", variable,
                ", levels=c(", paste(paste("'", levels, "'", sep=""), collapse=","), ")",
                ordered, ")", sep="")
            justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
            logger(paste(.activeDataSet,"$", name," <- ", command, sep=""))
            activeDataSet(.activeDataSet, flushModel=FALSE)
            }
        subOKCancelHelp()
        tkgrid(tklabel(subdialog, text="Old Levels", fg="blue"), 
            tklabel(subdialog, text="New order", fg="blue"), sticky="w")
        for (i in 1:nvalues){
            valVar <- paste("levelOrder", i, sep="")
            assign(valVar, tclVar(i))
            assign(paste("entry", i, sep=""), tkentry(subdialog, width="2", 
                textvariable=eval(parse(text=valVar))))
            tkgrid(tklabel(subdialog, text=old.levels[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
            }
        tkgrid(subButtonsFrame, sticky="w", columnspan=2)
        dialogSuffix(subdialog, focus=entry1, rows=nvalues+1, columns=2)
        }
    OKCancelHelp(helpSubject="factor")
    tkgrid(getFrame(variableBox), sticky="nw")
    tkgrid(tklabel(top, text="Name for factor"), sticky="w")
    tkgrid(factorNameField, sticky="w")
    tkgrid(tklabel(orderedFrame, text="Make ordered factor"), orderedCheckBox, sticky="w")
    tkgrid(orderedFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=5, columns=1, preventGrabFocus=TRUE)
    }

standardize <- function(X){
#    if (!checkActiveDataSet()) return()
#    if (!checkNumeric()) return()
    initializeDialog(title="Standardize Variables")
    xBox <- variableListBox(top, Numeric(), title="Variables (pick one or more)",
        selectmode="multiple")
    onOK <- function(){
        x <- getSelection(xBox)
        closeDialog()
        if (length(x) == 0) {
            errorCondition(recall=standardize, message="You must select one or more variables.")
            return()
            }
        xx <- paste('"', x, '"', sep="")
        .activeDataSet <- ActiveDataSet()
        command <- paste("scale(", .activeDataSet, "[,c(", paste(xx, collapse=","),
            ")])", sep="")
        assign(".Z", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".Z <- ", command, sep=""))
        for (i in 1:length(x)){
            Z <- paste("Z.", x[i], sep="")
            if (is.element(Z, Variables())) {
                if ("no" == tclvalue(checkReplace(Z))){
                    if (GrabFocus()) tkgrab.release(top)
                    tkdestroy(top)
                    next
                    }
                }
            justDoIt(paste(.activeDataSet, "$", Z, " <- .Z[,", i, "]", sep=""))
            logger(paste(.activeDataSet, "$", Z, " <- .Z[,", i, "]", sep=""))
            }
        remove(.Z, envir=.GlobalEnv)   
        logger("remove(.Z)")
        activeDataSet(.activeDataSet, flushModel=FALSE)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="scale")
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }

helpDataSet <- function(){
#    if (!checkActiveDataSet()) return()
    .activeDataSet <- ActiveDataSet()
    if (as.numeric(R.Version()$major) >= 2) doItAndPrint(paste('help("', .activeDataSet, '")', sep=""))
    else {
        justDoIt(paste("help('", .activeDataSet, "')", sep=""))
        logger(paste('help("', .activeDataSet, '")', sep=""))
        }
    NULL
    }
    
variablesDataSet <- function(){
#    if (!checkActiveDataSet()) return()
    doItAndPrint(paste("names(", ActiveDataSet(), ")", sep=""))
    }

exportDataSet <- function() {
#    if (!checkActiveDataSet()) return()
    dsname <- activeDataSet()
    initializeDialog(title="Export Active Data Set")
    checkBoxes(frame="optionsFrame", boxes=c("colnames", "rownames", "quotes"),
        initialValues=rep(1,3), labels=c("Write variable names:", "Write row names:", "Quotes around character values:"))
    missingVariable <- tclVar("NA")
    missingEntry <- tkentry(optionsFrame, width="8", textvariable=missingVariable)
    radioButtons(name="delimiter", buttons=c("spaces", "tabs", "commas"), labels=c("Spaces", "Tabs", "Commas"),
        title="Field Separator")
    onOK <- function(){
        closeDialog()
        col <- tclvalue(colnamesVariable) == 1
        row <- tclvalue(rownamesVariable) == 1
        quote <- tclvalue(quotesVariable) == 1
        delim <- tclvalue(delimiterVariable)
        missing <- tclvalue(missingVariable)
        sep <- if (delim == "tabs") "\\t"
            else if (delim == "spaces") " "
            else ","
        saveFile <- tclvalue(tkgetSaveFile(filetypes='{"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}} {"All Files" {"*"}}',
            defaultextension="txt", initialfile=paste(dsname, ".txt", sep="")))
        if (saveFile == "") {
            tkfocus(CommanderWindow())
            return()
            }
        command <- paste("write.table(", dsname, ', "', saveFile, '", sep="', sep, 
            '", col.names=', col, ", row.names=", row, ", quote=", quote,
            ', na="', missing, '")', sep="")           
        justDoIt(command)
        logger(command)
        Message(paste("Active dataset exported to file", saveFile), type="note")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="write.table")
    tkgrid(tklabel(optionsFrame, text="Missing values:"), missingEntry, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(delimiterFrame, stick="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1)
    }

filterNA <- function(){
#    if (!checkActiveDataSet()) return()
    dataSet <- activeDataSet()
    initializeDialog(title="Remove Missing Data")
    allVariablesFrame <- tkframe(top)
    allVariables <- tclVar("1")
    allVariablesCheckBox <- tkcheckbutton(allVariablesFrame, variable=allVariables)
    variablesBox <- variableListBox(top, Variables(), selectmode="multiple", initialSelection=NULL,
        title="Variables (select one or more)")
    newDataSetName <- tclVar("<same as active data set>")
    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <- tkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
    onOK <- function(){
        x <- getSelection(variablesBox)
        closeDialog()
        newName <- trim.blanks(tclvalue(newDataSetName))
        .activeDataSet <- ActiveDataSet()
        if (newName == "<same as active data set>") newName <- .activeDataSet
        if (!is.valid.name(newName)){
            errorCondition(recall=filterNA,
                message=paste('"', newName, '" is not a valid name.', sep=""))
            return()
            }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName, "Data set"))){
                filterNA()
                return()
                }
            }
        if (tclvalue(allVariables) == "1"){
            command <- paste(newName, " <- na.omit(", .activeDataSet, ")", sep="")
            logger(command)
            justDoIt(command)
            activeDataSet(newName)
            tkfocus(CommanderWindow())
            }
        else {
            if (length(x) == 0) {
                errorCondition(recall=filterNA, message="No variables were selected.")
                return()
                }
            x <- paste('"', x, '"', sep="")
            command <- paste(newName, " <- na.omit(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')])', sep="")
            logger(command)
            justDoIt(command)
            activeDataSet(newName)
            tkfocus(CommanderWindow())
            }
        }
    OKCancelHelp(helpSubject="na.omit")
    tkgrid(tklabel(allVariablesFrame, text="Include all variables"), 
        allVariablesCheckBox, sticky="w")
    tkgrid(allVariablesFrame, sticky="w")
    tkgrid(tklabel(top, text="   OR", fg="red"), sticky="w")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(tklabel(dataSetNameFrame, text="Name for new data set"), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

subsetDataSet <- function(){
#    if (!checkActiveDataSet()) return()
    dataSet <- activeDataSet()
    initializeDialog(title="Subset Data Set")
    allVariablesFrame <- tkframe(top)
    allVariables <- tclVar("1")
    allVariablesCheckBox <- tkcheckbutton(allVariablesFrame, variable=allVariables)
    variablesBox <- variableListBox(top, Variables(), selectmode="multiple",
        initialSelection=NULL, title="Variables (select one or more)")
    subsetVariable <- tclVar("<all cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    newDataSetName <- tclVar("<same as active data set>")
    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <- tkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
    onOK <- function(){
        newName <- trim.blanks(tclvalue(newDataSetName))
        if (newName == "<same as active data set>") newName <- ActiveDataSet()
        if (!is.valid.name(newName)){
            errorCondition(recall=subsetDataSet,
                message=paste('"', newName, '" is not a valid name.', sep=""))
            return()
            }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName))){
                subsetDataSet()
                return()
                }
            }
        selectVars <- if (tclvalue(allVariables) == "1") ""
            else {
                x <- getSelection(variablesBox)
                if (0 > length(x)) {
                    errorCondition(recall=subsetDataSet,
                        message="No variables were selected.")
                    return()
                    }
                paste(", select=c(", paste(x, collapse=","), ")", sep="")
                }
        closeDialog()
        cases <- tclvalue(subsetVariable)
        selectCases <- if (cases == "<all cases>") ""
            else paste(", subset=", cases, sep="")
        if (selectVars == "" && selectCases ==""){
            errorCondition(recall=subsetDataSet,
                message="New data set same as active data set.")
            return()
            }
        command <- paste(newName, " <- subset(", ActiveDataSet(), selectCases, selectVars, ")",
            sep="")
        logger(command)
        justDoIt(command)
        activeDataSet(newName)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="subset")
    tkgrid(tklabel(allVariablesFrame, text="Include all variables"), 
        allVariablesCheckBox, sticky="w")
    tkgrid(allVariablesFrame, sticky="w")
    tkgrid(tklabel(top, text="   OR", fg="red"), sticky="w")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(tklabel(dataSetNameFrame, text="Name for new data set"), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
    }

setCaseNames <- function(){
#    if (!checkActiveDataSet()) return()
    dataSet <- activeDataSet()
    initializeDialog(title="Set Case Names")
    variablesBox <- variableListBox(top, Variables(), title="Select variable containing row names",
        initialSelection=NULL)
    onOK <- function(){
        variable <- getSelection(variablesBox)
        closeDialog()
        if (length(variable) == 0) {
            errorCondition(recall=setCaseNames, message="You must select a variable.")
            return()
            }
        var <- eval(parse(text=paste(dataSet, "$", variable, sep="")), envir=.GlobalEnv)
        if (length(var) != length(unique(var))){
            errorCondition(recall=setCaseNames, message="Case names must be unique.")
            return()
            }
        command <- paste("row.names(", dataSet, ") <- as.character(", dataSet, "$", variable, ")", sep="")
        justDoIt(command)
        logger(command)
        eval(parse(text=paste(dataSet, "$", variable, "<- NULL", sep="")), envir=.GlobalEnv)
        logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
        activeDataSet(dataSet, flushModel=FALSE)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="row.names")  
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1)
    }
    
renameVariables <- function(){
#    if (!checkActiveDataSet()) return()
    initializeDialog(title="Rename Variables")
    variableBox <- variableListBox(top, Variables(), title="Variables (pick one or more)",
        selectmode="multiple", initialSelection=NULL)
    onOK <- function(){
        variables <- getSelection(variableBox)
        closeDialog()
        nvariables <- length(variables)
        if (nvariables < 1) {
            errorCondition(recall=renameVariables, message="No variables selected.")
            return()
            }
        .activeDataSet <- ActiveDataSet()
        unordered.names <- names(eval(parse(text=.activeDataSet)))
        which.variables <- match(variables, unordered.names)
        initializeDialog(subdialog, title="Variable Names")
        newnames <- rep("", nvariables)
        onOKsub <- function() {
            closeDialog(subdialog)
            for (i in 1:nvariables){
                newnames[i] <- eval(parse(text=paste("tclvalue(newName", i, ")", sep="")))
                }
            if (any(newnames == "")){
                errorCondition(recall=renameVariables, message="A variable name is empty.")
                return()
                }
            test.names <- newnames == make.names(newnames)
            if (!all(test.names)){
                errorCondition(recall=renameVariables,
                    message=paste("The following variable names are not valid:\n",
                    paste(newnames[!test.names], collapse=", ")))
                return()
                }                
            all.names <- eval(parse(text=paste("names(", .activeDataSet, ")")))
            all.names[which.variables] <- newnames
            if (length(unique(all.names)) != length(all.names)){
                errorCondition(recall=renameVariables, message="Variable names are not unique")
                return()
                }
            command <- paste("names(", .activeDataSet, ")[c(", paste(which.variables, collapse=","),
                ")] <- c(", paste('"', newnames, '"', collapse=",", sep=""), ")", sep="")
            justDoIt(command)
            logger(command)
            activeDataSet(.activeDataSet, flushModel=FALSE)
            tkfocus(CommanderWindow())
            }
        subOKCancelHelp()
        tkgrid(tklabel(subdialog, text="Old Name", fg="blue"), 
            tklabel(subdialog, text="New name", fg="blue"), sticky="w")
        for (i in 1:nvariables){
            valVar <- paste("newName", i, sep="")
            assign(valVar, tclVar(""))
            assign(paste("entry", i, sep=""), tkentry(subdialog, width="20", 
                textvariable=eval(parse(text=valVar))))
            tkgrid(tklabel(subdialog, text=variables[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
            }
        tkgrid(subButtonsFrame, sticky="w", columnspan=2)
        dialogSuffix(subdialog, rows=nvariables+2, columns=2, focus=entry1, onOK=onOKsub)                 
        }
    OKCancelHelp(helpSubject="names")
    tkgrid(getFrame(variableBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }

setContrasts <- function(){
#    if (!checkActiveDataSet()) return()
#    if (!checkFactors()) return()
    initializeDialog(title="Set Contrasts for Factor")
    variableBox <- variableListBox(top, Factors(), title="Factor (pick one)")
    radioButtons(name="contrasts", buttons=c("treatment", "sum", "helmert", "poly", "specify"), 
        values=c("contr.Treatment", "contr.Sum", "contr.helmert", "contr.poly", "specify"),
        labels=c("Treatment (dummy) contrasts", "Sum (deviation) contrasts", "Helmert contrasts",
            "Polynomial contrasts", "Other (specify)"), title="Contrasts")
    onOK <- function(){
        variable <- getSelection(variableBox)
        closeDialog()
        if (length(variable) == 0) {
            errorCondition(recall=setContrasts, message="You must select a variable.")
            return()
            }
        contrasts <- tclvalue(contrastsVariable)
        if (contrasts != "specify"){
            command <- paste("contrasts(", ActiveDataSet(), "$", variable, ') <- "', contrasts, '"', sep="")
            justDoIt(command)
            logger(command)
            activeDataSet(ActiveDataSet())
            tkfocus(CommanderWindow())
            }
        else{
            initializeDialog(subdialog, title="Specify Contrasts")
            tkgrid(tklabel(subdialog, text="Enter Contrast Coefficients", fg="blue"), sticky="w")
            env <- environment()
            tableFrame <- tkframe(subdialog)
            row.names <- eval(parse(text=paste("levels(", ActiveDataSet(), "$", variable, ")")))
            row.names <- substring(paste(abbreviate(row.names, 12), "            "), 1, 12)
            nrows <- length(row.names)
            ncols <- nrows - 1
            make.col.names <- "tklabel(tableFrame, text='Contrast Name:')"
            for (j in 1:ncols) {
                varname <- paste(".col.", j, sep="")
                assign(varname, tclVar(paste(".", j, sep="")), envir=env)
                make.col.names <- paste(make.col.names, ", ", 
                    "tkentry(tableFrame, width='12', textvariable=", varname, ")", sep="")
                }
            eval(parse(text=paste("tkgrid(", make.col.names, ", sticky='w')", sep="")), envir=env)
            for (i in 1:nrows){   
                make.row <- paste("tklabel(tableFrame, text='", row.names[i], "')")
                for (j in 1:ncols){
                    varname <- paste(".tab.", i, ".", j, sep="")
                    assign(varname, tclVar("0"), envir=env)
                    make.row <- paste(make.row, ", ", "tkentry(tableFrame, width='5', textvariable=", 
                        varname, ")", sep="")
                    }
                eval(parse(text=paste("tkgrid(", make.row, ", sticky='w')", sep="")), envir=env)
                }
            tkgrid(tableFrame, sticky="w")
            onOKsub <- function(){
                closeDialog(subdialog)
                cell <- 0
                values <- rep(NA, nrows*ncols)
                for (j in 1:ncols){
                    for (i in 1:nrows){
                        cell <- cell + 1
                        varname <- paste(".tab.", i, ".", j, sep="")
                        values[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
                        }
                    }
                values <- na.omit(values)
                if (length(values) != nrows*ncols){
                    errorCondition(subdialog, recall=setContrasts,
                        message=paste("Number of valid entries in contrast matrix(", length(values), ")\n",
                            "not equal to number of levels (", nrows,") * number of contrasts (", ncols,").", 
                            sep=""))
                    return()
                    }
                if (qr(matrix(values, nrows, ncols))$rank < ncols) {
                    errorCondition(subdialog, recall=setContrasts, message="Contrast matrix is not of full column rank")
                    return()
                    }  
                contrast.names <- rep("", ncols)
                for (j in 1:ncols){
                    varname <- paste(".col.", j, sep="")
                    contrast.names[j] <- eval(parse(text=paste("tclvalue(", varname,")", sep="")))
                    }
                if (length(unique(contrast.names)) < ncols) {
                    errorCondition(subdialog, recall=setContrasts, message="Contrast names must be unique") 
                    return()
                    }                    
                command <- paste("matrix(c(", paste(values, collapse=","), "), ", nrows, ", ", ncols,
                    ")", sep="")
                assign(".Contrasts", justDoIt(command), envir=.GlobalEnv)
                logger(paste(".Contrasts <- ", command, sep=""))
                command <- paste("colnames(.Contrasts) <- c(", 
                    paste("'", contrast.names, "'", sep="", collapse=", "), ")", sep="")
                justDoIt(command)
                logger(command)
                command <- paste("contrasts(", ActiveDataSet(), "$", variable, ") <- .Contrasts", sep="")
                justDoIt(command)
                logger(command)
                justDoIt("remove(.Contrasts, envir=.GlobalEnv)")   
                logger("remove(.Contrasts)") 
                activeDataSet(ActiveDataSet(), flushModel=FALSE)
                tkfocus(CommanderWindow())
                }
            subOKCancelHelp(helpSubject="contrasts")
            tkgrid(tableFrame, sticky="w")
            tkgrid(tklabel(subdialog, text=""))
            tkgrid(subButtonsFrame, sticky="w")
            dialogSuffix(subdialog, rows=5, columns=1, focus=subdialog)  
            }
        }
    OKCancelHelp(helpSubject="contrasts")
    tkgrid(getFrame(variableBox), sticky="nw")
    tkgrid(contrastsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1) 
    }
