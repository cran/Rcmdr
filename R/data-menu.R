# last modified 2 May 2004 by J. Fox

# Data menu dialogs

newDataSet <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "New Data Set")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter the name of a data set."),
                icon="error", type="ok", default="ok")
                newDataSet()
                return()
                }     
        if (!is.valid.name(dsnameValue)){
            tkmessageBox(message=paste('"', dsnameValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            newDataSet()
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                newDataSet()
                return()
                }
            }
        command <- "edit(as.data.frame(NULL))"
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        logger(paste(dsnameValue, " <- ", command, sep=""))
        activeDataSet(dsnameValue)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12" ,command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(edit.data.frame)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(entryDsname, sticky="w")
    tkgrid.configure(helpButton, sticky="e")    
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(entryDsname, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }

selectActiveDataSet <- function(){
    dataSets <- listDataSets()
    if (length(dataSets) == 0){
        tkmessageBox(message="There are no data sets from which to choose.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Select Data Set")
    dataSetsFrame <- tkframe(top)
    dataSetsBox <- tklistbox(dataSetsFrame, height=min(4, length(dataSets)),
        selectmode="single", background="white")
    dataSetsScroll <- tkscrollbar(dataSetsFrame, repeatinterval=5, 
        command=function(...) tkyview(dataSetsBox, ...))
    tkconfigure(dataSetsBox, yscrollcommand=function(...) tkset(dataSetsScroll, ...))
    for (ds in dataSets) tkinsert(dataSetsBox, "end", ds)
    tkselection.set(dataSetsBox, if (is.null(.activeDataSet)) 0 else which(.activeDataSet == dataSets) - 1)
    onOK <- function(){
        activeDataSet(dataSets[as.numeric(tkcurselection(dataSetsBox)) + 1])
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
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
        help(attach)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Data Sets (pick one)"), sticky="w")
    tkgrid(dataSetsBox, dataSetsScroll, sticky="nw")
    tkgrid(dataSetsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkgrid.configure(dataSetsScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(dataSetsBox, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
    
listDataSetsInPackages <- function() doItAndPrint("data()")

Recode <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    processRecode <- function(recode){
        parts <- strsplit(recode, "=")[[1]]
        if (length(grep(",", parts[1])) > 0) paste("c(", parts[1], ") = ", parts[2], sep="")
            else paste(parts, collapse="=")
        }
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Recode Variable")
    variablesFrame <- tkframe(top)
    variablesListFrame <- tkframe(variablesFrame)
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="single", background="white", exportselection="FALSE")
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    tkselection.set(variablesBox, 0)
    newVariableName <- tclVar("variable")
    newVariable <- tkentry(variablesFrame, width="20", textvariable=newVariableName)
    recodesFrame <- tkframe(top)
    recodes <- tktext(recodesFrame, bg="white", font=tkfont.create(family="courier", size=10), 
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
        variable <- as.character(tkget(variablesBox, "active"))
        newVar <- trim.blanks(tclvalue(newVariableName))
        if (!is.valid.name(newVar)){
            tkmessageBox(message=paste('"', newVar, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Recode()
            return()
            }
        asFactor <- tclvalue(asFactorVariable) == "1"
        recode.directives <- gsub("\n", "; ", tclvalue(tkget(recodes, "1.0", "end")))
        check.empty <- gsub(";", "", gsub(" ", "", recode.directives))
        if ("" == check.empty) {
            tkmessageBox(message="No recode directives specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Recode()
            return()
            }
        if (0 != length(grep("'", recode.directives))) {
            tkmessageBox(message='Use only double-quotes (" ") in recode directives', 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Recode()
            return()
            }
        if (is.element(newVar, .variables)) {
            if ("no" == tclvalue(checkReplace(newVar))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                Recode()
                return()
                }
            }
        recode.directives <- strsplit(recode.directives, ";")[[1]]
        recode.directives <- paste(sapply(recode.directives, processRecode), collapse=";") 
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        cmd <- paste("recode(", dataSet,"$",variable, ", '", recode.directives, 
            "', as.factor.result=", asFactor, ")", sep="")
        logger(paste(dataSet,"$",newVar, " <- ", cmd, sep=""))
#        justDoIt(paste(dataSet,"$",newVar, " <<- ", cmd, sep=""))
        justDoIt(paste(dataSet,"$",newVar, " <- ", cmd, sep=""))
        activeDataSet(dataSet)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKCancelFrame <- tkframe(top)
    OKbutton <- tkbutton(OKCancelFrame, text="OK", fg="darkgreen", width="12",command=onOK)
    cancelButton <- tkbutton(OKCancelFrame, text="Cancel", fg="red", width="12",command=onCancel)    
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(Recode)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)       
    tkgrid(tklabel(top, text="Variable to recode (pick one)"), 
        tklabel(top, text="Recode directives"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesListFrame, sticky="nw")
    tkgrid(tklabel(variablesFrame, text="New variable name"), sticky="w")
    tkgrid(newVariable, sticky="w")
    tkgrid(recodes, recodesYscroll, sticky="nw")
    tkgrid(recodesXscroll)
    tkgrid(variablesFrame, recodesFrame, sticky="nw")
    tkgrid(tklabel(asFactorFrame, text="Make new variable a factor"), asFactorCheckBox, 
        sticky="w")
    tkgrid(asFactorFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w") 
    tkgrid(OKCancelFrame, helpButton, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid.configure(recodesXscroll, sticky="ew")
    tkgrid.configure(recodesYscroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(recodes)
    tkwait.window(top)             
    }

Compute <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Compute New Variable")
    variablesFrame <- tkframe(top)
    variablesListFrame <- tkframe(variablesFrame)
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="browse", background="white")
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    newVariableName <- tclVar("variable")
    newVariable <- tkentry(variablesFrame, width="20", textvariable=newVariableName)
    computeFrame <- tkframe(top)
    computeVar <- tclVar("")
    compute <- tkentry(computeFrame, font=.logFont, width="30", textvariable=computeVar)
    computeXscroll <- tkscrollbar(computeFrame, repeatinterval=10,
        orient="horizontal", command=function(...) tkxview(compute, ...))
    tkconfigure(compute, xscrollcommand=function(...) tkset(computeXscroll, ...))
    onOK <- function(){
        newVar <- trim.blanks(tclvalue(newVariableName))
        if (!is.valid.name(newVar)){
            tkmessageBox(message=paste('"', newVar, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Compute()
            return()
            }
        express <- tclvalue(computeVar)
        check.empty <- gsub(";", "", gsub(" ", "", express))
        if ("" == check.empty) {
            tkmessageBox(message="No expression specified.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Compute()
            return()
            }
        if (is.element(newVar, .variables)) {
            if ("no" == tclvalue(checkReplace(newVar))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                Compute()
                return()
                }
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        logger(paste(dataSet,"$",newVar, " <- ", express, sep=""))
#        justDoIt(paste(dataSet,"$",newVar, " <<- with(", .activeDataSet,
        justDoIt(paste(dataSet,"$",newVar, " <- with(", .activeDataSet,
            " ,", express, ")"))
        activeDataSet(dataSet)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKCancelFrame <- tkframe(top)
    OKbutton <- tkbutton(OKCancelFrame, text="OK", fg="darkgreen", width="12", command=onOK)
    cancelButton <- tkbutton(OKCancelFrame, text="Cancel", fg="red", width="12",command=onCancel)  
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help("Compute")
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)         
    tkgrid(tklabel(variablesFrame, text="Current variables (list only)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="w")
    tkgrid(variablesListFrame, sticky="w")
    tkgrid(tklabel(variablesFrame, text="New variable name"), sticky="w")
    tkgrid(newVariable, sticky="w")
    tkgrid(tklabel(computeFrame, text="Expression to compute"), sticky="w")
    tkgrid(compute, sticky="w")
    tkgrid(computeXscroll, sticky="ew")
    tkgrid(variablesFrame, computeFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w") 
    tkgrid(OKCancelFrame, helpButton, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e") 
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)   
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(compute)
    tkwait.window(top)
    }

deleteVariable <- function(){
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Delete Variables")
    variablesListFrame <- tkframe(top)
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="multiple", background="white")
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    onOK <- function(){
        variables <- .variables[as.numeric(tkcurselection(variablesBox)) + 1]
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
#            eval(parse(text=paste(dataSet, "$", variable, "<<- NULL", sep="")), envir=.GlobalEnv)
            eval(parse(text=paste(dataSet, "$", variable, "<- NULL", sep="")), envir=.GlobalEnv)
            logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
            }
        activeDataSet(dataSet)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12",command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)    
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help("NULL")
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)       
    tkgrid(tklabel(top, text="Variable(s) to delete (pick one or more)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesListFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)   
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

readDataSet <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Read Data From Text File")
    optionsFrame <- tkframe(top)
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(optionsFrame, width="20", textvariable=dsname)
    headerVariable <- tclVar("1")
    headerCheckBox <- tkcheckbutton(optionsFrame, variable=headerVariable)
    delimiterFrame <- tkframe(optionsFrame)
    delimiterVariable <- tclVar("whitespace")
    whitespaceButton <- tkradiobutton(delimiterFrame, variable=delimiterVariable, value="whitespace")
    commasButton <- tkradiobutton(delimiterFrame, variable=delimiterVariable, value="commas")
    otherButton <- tkradiobutton(delimiterFrame, variable=delimiterVariable, value="other")
    otherVariable <- tclVar("")
    otherEntry <- tkentry(delimiterFrame, width="4", textvariable=otherVariable) 
    decimalFrame <- tkframe(optionsFrame)
    decimalVariable <- tclVar("period")
    periodButton <- tkradiobutton(decimalFrame, variable=decimalVariable, value="period")
    commaButton <- tkradiobutton(decimalFrame, variable=decimalVariable, value="comma")  
    missingVariable <- tclVar("NA")
    missingEntry <- tkentry(optionsFrame, width="8", textvariable=missingVariable)    
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter a name for the data set."),
                icon="error", type="ok", default="ok")
                readDataSet()
                return()
                }
        if (!is.valid.name(dsnameValue)){
            tkmessageBox(message=paste('"', dsnameValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            readDataSet()
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                readDataSet()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(filetypes='{"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}} {"All Files" {"*"}}'))
        if (file == "") {
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            return()
            }
        head <- tclvalue(headerVariable) == "1"
        delimiter <- tclvalue(delimiterVariable)
        del <- if (delimiter == "whitespace") ""
            else if (delimiter == "commas") ","
            else tclvalue(otherVariable)
        miss <- tclvalue(missingVariable)
        dec <- if (tclvalue(decimalVariable) == "period") "." else ","
        command <- paste('read.table("', file,'", header=', head, 
            ', sep="', del, '", na.strings="', miss, '", dec="', dec, '", strip.white=TRUE)', sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        activeDataSet(dsnameValue)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    buttonsFrame <- tkframe(top)  
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(read.table)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(optionsFrame, text="Enter name for data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Variable names in file:"), headerCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Missing data indicator:"), missingEntry, sticky="w")
    tkgrid(tklabel(delimiterFrame, text="Field Separator", fg="blue"), sticky="w")
    tkgrid(tklabel(delimiterFrame, text="White space"), whitespaceButton, sticky="w")
    tkgrid(tklabel(delimiterFrame, text="Commas"), commasButton, sticky="w")
    tkgrid(tklabel(delimiterFrame, text="Other"), otherButton, 
        tklabel(delimiterFrame, text="  Specify:"), otherEntry, sticky="w")
    tkgrid(delimiterFrame, sticky="w", columnspan=2)
    tkgrid(tklabel(decimalFrame, text="Decimal-Point Character", fg="blue"), sticky="w", columnspan=2)
    tkgrid(tklabel(decimalFrame, text="Period [.]"), periodButton, sticky="w")
    tkgrid(tklabel(decimalFrame, text="Comma [,]"), commaButton, sticky="w")
    tkgrid(decimalFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(helpButton, sticky="e")   
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK) 
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }
    
readDataFromPackage <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Read Data From Package")
    dsname <- tclVar("")
    enterFrame <- tkframe(top)
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    packages <- .packages()
    packagesFrame <- tkframe(top)
    packagesBox <- tklistbox(packagesFrame, height=min(4, length(packages)),
        selectmode="single", background="white", exportselection="FALSE")
    packagesScroll <- tkscrollbar(packagesFrame, repeatinterval=5, 
        command=function(...) tkyview(packagesBox, ...))
    tkconfigure(packagesBox, yscrollcommand=function(...) tkset(packagesScroll, ...))
    for (pack in packages) tkinsert(packagesBox, "end", pack)
    onOK <- function(){
        dsnameValue <- tclvalue(dsname)
        if (dsnameValue != ""){
            if (is.element(dsnameValue, listDataSets())) {
                if ("no" == tclvalue(checkReplace(dsnameValue))){
                    if (.grab.focus) tkgrab.release(top)
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
                tkmessageBox(message=paste("Data set", dsnameValue, "does not exist."),
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                readDataFromPackage()
                return()
                }
            activeDataSet(dsnameValue)
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            }
        else{
            packageName <- as.character(tkget(packagesBox, "active")) 
            save.options <- options(warn=-1)
            dataSets <- data(package=parse(text=packageName))$results[,3] 
            options(save.options)
            if (length(dataSets) == 0){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                tkmessageBox(message=paste("There are no data sets in package", packageName),
                    icon="error", type="ok", default="ok")
                    readDataFromPackage()
                    return()
                }
            subdialog <- tktoplevel()
            tkwm.title(subdialog, "Select Data Set")
            dsFrame <- tkframe(subdialog)
            dsBox <- tklistbox(dsFrame, height=min(4, length(dataSets)),
                selectmode="single", background="white", exportselection="FALSE")
            dsScroll <- tkscrollbar(dsFrame, repeatinterval=5, 
                command=function(...) tkyview(dsBox, ...))
            tkconfigure(dsBox, yscrollcommand=function(...) tkset(dsScroll, ...))
            for (ds in dataSets) tkinsert(dsBox, "end", ds)
            onOKsub <- function() {
                dsnameValue <- as.character(tkget(dsBox, "active"))
                if (is.element(dsnameValue, listDataSets())) {
                    if ("no" == tclvalue(checkReplace(dsnameValue))){
                        if (.grab.focus) tkgrab.release(subdialog)
                        tkdestroy(subdialog)
                        tkdestroy(top)
                        readDataFromPackage()
                        return()
                        }
                    }
                command <- paste("data(", dsnameValue, ', package="', packageName, '")', sep="")
                justDoIt(command)
                logger(command)
                activeDataSet(dsnameValue)                
                if (.grab.focus) tkgrab.release(subdialog)
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            onCancelSub <- function() {
                if (.grab.focus) tkgrab.release(subdialog)  
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            subButtonFrame <- tkframe(subdialog)
            OKSubButton <- tkbutton(subButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
            cancelSubButton <- tkbutton(subButtonFrame, text="Cancel", fg="red", width="12",command=onCancelSub)
            labelFrame <- tkframe(subdialog)
            tkgrid(dsBox, dsScroll, sticky="nw")
            tkgrid(tklabel(labelFrame, text="Package: "), tklabel(labelFrame, text=packageName, fg="red"),
                sticky="w")
            tkgrid(labelFrame, sticky="w")
            tkgrid(tklabel(subdialog, text="Select data set"), sticky="w")
            tkgrid(dsFrame, sticky="w")
            tkgrid(OKSubButton, cancelSubButton, sticky="w")
            tkgrid(subButtonFrame, sticky="w")
            tkgrid.configure(dsScroll, sticky="ns")
            for (row in 0:3) tkgrid.rowconfigure(subdialog, row, weight=0)
            for (col in 0:1) tkgrid.columnconfigure(subdialog, col, weight=0)
            .Tcl("update idletasks")
            tkwm.resizable(subdialog, 0, 0)
            tkbind(subdialog, "<Return>", onOKsub)
            if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
            tkbind(dsBox, "<Double-ButtonPress-1>", onOKsub)
            tkselection.set(dsBox, 0)
            if (.grab.focus) tkgrab.release(top)
            tkwm.deiconify(subdialog)
            if (.grab.focus) tkgrab.set(subdialog)
            tkfocus(subdialog)
            tkwait.window(subdialog)
            tkdestroy(top)           
            }
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(data)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(packagesBox, packagesScroll, sticky="nw")
    tkgrid(tklabel(top, text="Enter name of data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(top, text="OR", fg="red"), sticky="w")
    tkgrid(tklabel(top, text="Select package:"), packagesFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(packagesScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(entryDsname, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(packagesBox, "<Double-ButtonPress-1>", onOK)
    tkselection.set(packagesBox, 0)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }

importSPSS <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Import SPSS Data Set")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    asFactor <- tclVar("1")
    asFactorCheckBox <- tkcheckbutton(top, variable=asFactor)
    maxLevels <- tclVar("Inf")
    entryMaxLevels <- tkentry(top, width="5", textvariable=maxLevels)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter the name of a data set."),
                icon="error", type="ok", default="ok")
                importSPSS()
                return()
                }
        if (!is.valid.name(dsnameValue)){
            tkmessageBox(message=paste('"', dsnameValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            importSPSS()
            return()
            }
                     
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                importSPSS()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(
            filetypes='{"SPSS save files" {".sav" ".SAV"}} {"SPSS portable files" {".por" ".POR"}} {"All Files" {"*"}}'))
        if (file == "") {
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            return()
            }
        factor <- tclvalue(asFactor) == "1"
        levels <- as.numeric(tclvalue(maxLevels))
        command <- paste('read.spss("', file,'", use.value.labels=', factor,
            ", max.value.labels=", levels, ", to.data.frame=TRUE)", sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        activeDataSet(dsnameValue)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(read.spss)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(top, text="Convert value labels\nto factor levels", justify="left"), 
        asFactorCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Maximum number\nof value labels\nfor factor conversion", justify="left"), 
        entryMaxLevels, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(entryDsname, sticky="w")
    tkgrid.configure(asFactorCheckBox, sticky="w")
    tkgrid.configure(entryMaxLevels, sticky="w")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(entryDsname, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }

importMinitab <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Import Minitab Data Set")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter the name of a data set."),
                icon="error", type="ok", default="ok")
                importMinitab()
                return()
                }     
        if (!is.valid.name(dsnameValue)){
            tkmessageBox(message=paste('"', dsnameValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            importMinitab()
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                importMinitab()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(
            filetypes='{"Minitab portable files" {".mtp" ".MTP"}} {"All Files" {"*"}}'))
        if (file == "") {
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            return()
            }
        command <- paste('read.mtp("', file,'")', sep="")
        datalist <- justDoIt(command)
        lengths <- sapply(datalist, length)
        datalist <- datalist[lengths != 0]
        lengths <- lengths[lengths != 0]
        if (!all(lengths == length(datalist[[1]]))){
            tkmessageBox(message=
                paste("Minitab data set contains elements of unequal length.\nData set cannot be converted."),
                icon="error", type="ok")
            tkdestroy(top)
            tkfocus(.commander)
            return()
            }
        assign(dsnameValue, as.data.frame(datalist), envir=.GlobalEnv)
        logger(paste(dsnameValue, " <- as.data.frame(", command, ")", sep=""))
        activeDataSet(dsnameValue)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12" ,command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(read.mtp)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(entryDsname, sticky="w")
    tkgrid.configure(helpButton, sticky="e")    
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(entryDsname, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }

numericToFactor <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
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
    tkwm.title(top, "Convert Numeric Variable to Factor")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .numeric) tkinsert(variableBox, "end", var)
    levelsFrame <- tkframe(top)
    levelsVariable <- tclVar("names")
    numbersButton <- tkradiobutton(levelsFrame, variable=levelsVariable, value="numbers")
    namesButton <- tkradiobutton(levelsFrame, variable=levelsVariable, value="names")
    factorName <- tclVar("<same as variable>")
    factorNameField <- tkentry(top, width="20", textvariable=factorName)
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        name <- trim.blanks(tclvalue(factorName))
        if (name == "<same as variable>") name <- variable
        if (!is.valid.name(name)){
            tkmessageBox(message=paste('"', name, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            numericToFactor()
            return()
            }
        if (is.element(name, .variables)) {
            if ("no" == tclvalue(checkReplace(name))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                numericToFactor()
                return()
                }
            }
        levelsType <- tclvalue(levelsVariable)
        if (levelsType == "names"){
            values <- sort(unique(eval(parse(text=paste(.activeDataSet, "$", variable, sep="")),
                envir=.GlobalEnv)))
            nvalues <- length(values)
            if (nvalues > 30) {
                tkmessageBox(message=paste("Number of levels (", nvalues, ") too large.", sep=""),
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                numericToFactor()
                return()
                }
            subdialog <- tktoplevel()
            tkwm.title(subdialog, "Level Names")
            names <- rep("", nvalues)
            onOKsub <- function() {
                for (i in 1:nvalues){
                    names[i] <- eval(parse(text=paste("tclvalue(levelName", i, ")", sep="")))
                    }
                if (length(unique(names)) != nvalues){
                    tkmessageBox(message="Levels names are not unique.",
                        icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    numericToFactor()
                    return()
                    }
                if (any(names == "")){
                    tkmessageBox(message="A level name is empty.",
                        icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    numericToFactor()
                    return()
                    }
                command <- paste("factor(", .activeDataSet, "$", variable,
                    ", labels=c(", paste(paste("'", names, "'", sep=""), collapse=","), "))", sep="")
#                justDoIt(paste(.activeDataSet, "$", name, " <<- ", command, sep=""))
                justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
                logger(paste(.activeDataSet,"$", name," <- ", command, sep=""))
                activeDataSet(.activeDataSet)
                if (.grab.focus) tkgrab.release(subdialog)
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            onCancelSub <- function() {
                if (.grab.focus) tkgrab.release(subdialog)  
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            OKSubButton <- tkbutton(subdialog, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
            cancelSubButton <- tkbutton(subdialog, text="Cancel", fg="red", width="12",command=onCancelSub)
            tkgrid(tklabel(subdialog, text="Numeric value"), tklabel(subdialog, text="Level name"), sticky="w")
            for (i in 1:nvalues){
                valVar <- paste("levelName", i, sep="")
                assign(valVar, tclVar(""))
                assign(paste("entry", i, sep=""), tkentry(subdialog, width="20", 
                    textvariable=eval(parse(text=valVar))))
                tkgrid(tklabel(subdialog, text=values[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
                }
            tkgrid(OKSubButton, cancelSubButton, sticky="w")
            for (row in 0:(nvalues + 1)) tkgrid.rowconfigure(subdialog, row, weight=0)
            for (col in 0:1) tkgrid.columnconfigure(subdialog, col, weight=0)
            .Tcl("update idletasks")
            tkwm.resizable(subdialog, 0, 0)
            tkbind(subdialog, "<Return>", onOKsub)
            if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)           
            tkwm.deiconify(subdialog)
            if (.grab.focus) tkgrab.set(subdialog)
            tkfocus(entry1)
            tkwait.window(subdialog)
            }
        else{
            command <- paste("as.factor(", .activeDataSet, "$", variable, ")", sep="")
#            justDoIt(paste(.activeDataSet, "$", name, " <<- ", command, sep=""))
            justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
            logger(paste(.activeDataSet, "$", name," <- ", command, sep=""))
            activeDataSet(.activeDataSet)
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            }
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
        help(factor)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), 
        tklabel(top, text="Factor Levels", fg="blue"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(tklabel(levelsFrame, text="Supply level names"), namesButton, sticky="w")
    tkgrid(tklabel(levelsFrame, text="Use numbers"), numbersButton, sticky="w")
    tkgrid(variableFrame, levelsFrame, sticky="nw")
    tkgrid(tklabel(top, text="Name for factor"), sticky="w")
    tkgrid(factorNameField, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    tkgrid.configure(numbersButton, sticky="w")
    tkgrid.configure(namesButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)   # causes problems ?
    tkfocus(top)
    tkwait.window(top)
    }

reorderFactor <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
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
    tkwm.title(top, "Reorder Factor Levels")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .factors) tkinsert(variableBox, "end", var)
    orderedFrame <- tkframe(top)
    orderedVariable <- tclVar("0")
    orderedCheckBox <- tkcheckbutton(orderedFrame, variable=orderedVariable)
    factorName <- tclVar("<same as original>")
    factorNameField <- tkentry(top, width="20", textvariable=factorName)
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        name <- trim.blanks(tclvalue(factorName))
        if (name == "<same as original>") name <- variable
        if (!is.valid.name(name)){
            tkmessageBox(message=paste('"', name, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            reorderFactor()
            return()
            }
        if (is.element(name, .variables)) {
            if ("no" == tclvalue(checkReplace(name))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                reorderFactor()
                return()
                }
            }
        old.levels <- eval(parse(text=paste("levels(", .activeDataSet, "$", variable, ")", 
            sep="")), envir=.GlobalEnv)
        nvalues <- length(old.levels)
        ordered <- tclvalue(orderedVariable)
        if (nvalues > 30) {
            tkmessageBox(message=paste("Number of levels (", nvalues, ") too large.", sep=""),
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            reorderFactor()
            return()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Reorder Levels")
        order <- 1:nvalues
        onOKsub <- function() {
            for (i in 1:nvalues){
                order[i] <- as.numeric(eval(parse(text=paste("tclvalue(levelOrder", i, ")", sep=""))))
                }
            if (any(sort(order) != 1:nvalues)){
                tkmessageBox(message=paste("Order of levels must include all integers from 1 to ",
                    nvalues, sep=""), icon="error", type="ok")
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                reorderFactor()
                return()
                }
            levels <- old.levels[order(order)]
            ordered <- if (ordered == "1") ", ordered=TRUE" else ""
            command <- paste("factor(", .activeDataSet, "$", variable,
                ", levels=c(", paste(paste("'", levels, "'", sep=""), collapse=","), ")",
                ordered, ")", sep="")
#            justDoIt(paste(.activeDataSet, "$", name, " <<- ", command, sep=""))
            justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
            logger(paste(.activeDataSet,"$", name," <- ", command, sep=""))
            activeDataSet(.activeDataSet)
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            }
        onCancelSub <- function() {
            if (.grab.focus) tkgrab.release(subdialog)  
            tkfocus(.commander)
            tkdestroy(subdialog)
            }
        OKSubButton <- tkbutton(subdialog, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subdialog, text="Cancel", fg="red", width="12",command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Old Levels", fg="blue"), 
            tklabel(subdialog, text="New order", fg="blue"), sticky="w")
        for (i in 1:nvalues){
            valVar <- paste("levelOrder", i, sep="")
            assign(valVar, tclVar(i))
            assign(paste("entry", i, sep=""), tkentry(subdialog, width="2", 
                textvariable=eval(parse(text=valVar))))
            tkgrid(tklabel(subdialog, text=old.levels[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
            }
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        for (row in 0:(nvalues + 1)) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:1) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkdestroy(top)           
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
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
        help(factor)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Factor (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="nw")
    tkgrid(tklabel(top, text="Name for factor"), sticky="w")
    tkgrid(factorNameField, sticky="w")
    tkgrid(tklabel(orderedFrame, text="Make ordered factor"), orderedCheckBox, sticky="w")
    tkgrid(orderedFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)  # causes problems ?
    tkfocus(top)
    tkwait.window(top)
    }

standardize <- function(X){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
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
    tkwm.title(top, "Standardize Variables")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        xx <- paste('"', x, '"', sep="")
        command <- paste("scale(", .activeDataSet, "[,c(", paste(xx, collapse=","),
            ")])", sep="")
        assign(".Z", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".Z <- ", command, sep=""))
        for (i in 1:length(x)){
            Z <- paste("Z.", x[i], sep="")
            if (is.element(Z, .variables)) {
                if ("no" == tclvalue(checkReplace(Z))){
                    if (.grab.focus) tkgrab.release(top)
                    tkdestroy(top)
                    next
                    }
                }
#            justDoIt(paste(.activeDataSet, "$", Z, " <<- .Z[,", i, "]", sep=""))
            justDoIt(paste(.activeDataSet, "$", Z, " <- .Z[,", i, "]", sep=""))
            logger(paste(.activeDataSet, "$", Z, " <- .Z[,", i, "]", sep=""))
            }
        remove(.Z, envir=.GlobalEnv)   
        logger("remove(.Z)")
        activeDataSet(.activeDataSet)
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
        help(scale)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick one or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

helpDataSet <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    justDoIt(paste("help(", .activeDataSet, ")", sep=""))
    logger(paste("help(", .activeDataSet, ")", sep=""))
    }
    
variablesDataSet <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    doItAndPrint(paste("names(", .activeDataSet, ")", sep=""))
    }

exportDataSet <- function() {
    dsname <- activeDataSet()
    if (dsname == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Export Active Data Set")
    optionsFrame <- tkframe(top)
    colnames <- tclVar("1")
    colnamesCheckBox <- tkcheckbutton(optionsFrame, variable=colnames)
    rownames <- tclVar("1")
    rownamesCheckBox <- tkcheckbutton(optionsFrame, variable=rownames)
    quotes <- tclVar("1")
    quotesCheckBox <- tkcheckbutton(optionsFrame, variable=quotes)
    delimiter <- tclVar("spaces")
    spacesButton <- tkradiobutton(optionsFrame, variable=delimiter, value="spaces")
    tabsButton <- tkradiobutton(optionsFrame, variable=delimiter, value="tabs")
    commasButton <- tkradiobutton(optionsFrame, variable=delimiter, value="commas")
    missingVariable <- tclVar("NA")
    missingEntry <- tkentry(optionsFrame, width="8", textvariable=missingVariable)
    onOK <- function(){
        col <- tclvalue(colnames) == 1
        row <- tclvalue(rownames) == 1
        quote <- tclvalue(quotes) == 1
        delim <- tclvalue(delimiter)
        missing <- tclvalue(missingVariable)
        sep <- if (delim == "tabs") "\\t"
            else if (delim == "spaces") " "
            else ","
        saveFile <- tclvalue(tkgetSaveFile(filetypes='{"Text Files" {".txt" ".TXT" ".dat" ".DAT", ".csv", ".CSV"}} {"All Files" {"*"}}',
            defaultextension="txt", initialfile=paste(dsname, ".txt", sep="")))
        if (saveFile == "") {
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            return()
            }
        command <- paste("write.table(", dsname, ', "', saveFile, '", sep="', sep, 
            '", col.names=', col, ", row.names=", row, ", quote=", quote,
            ', na="', missing, '")', sep="")           
        justDoIt(command)
        logger(command)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    buttonsFrame <- tkframe(top)  
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(write.table)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(optionsFrame, text="Write variable names:"), colnamesCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Write row names:"), rownamesCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Quotes around character values:"), quotesCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Missing values:"), missingEntry, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Field Separator", fg="blue"), sticky="w", columnspan=2)
    tkgrid(tklabel(optionsFrame, text="Spaces"), spacesButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Tabs"), tabsButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Commas"), commasButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(helpButton, sticky="e") 
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)  
    tkbind(top, "<Return>", onOK) 
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

filterNA <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Remove Missing Data")
    allVariablesFrame <- tkframe(top)
    allVariables <- tclVar("1")
    allVariablesCheckBox <- tkcheckbutton(allVariablesFrame, variable=allVariables)
    variablesFrame <- tkframe(top)
    variablesBox <- tklistbox(variablesFrame, height=min(4, length(.variables)),
        selectmode="multiple", background="white", exportselection="FALSE")
    variablesScroll <- tkscrollbar(variablesFrame, repeatinterval=5, command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (var in .variables) tkinsert(variablesBox, "end", var)
    newDataSetName <- tclVar("<same as active data set>")
    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <- tkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
    onOK <- function(){
        newName <- trim.blanks(tclvalue(newDataSetName))
        if (newName == "<same as active data set>") newName <- .activeDataSet
        if (!is.valid.name(newName)){
            tkmessageBox(message=paste('"', newName, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            filterNA()
            return()
            }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                filterNA()
                return()
                }
            }
        if (tclvalue(allVariables) == "1"){
            command <- paste(newName, " <- na.omit(", .activeDataSet, ")", sep="")
            logger(command)
            justDoIt(command)
            activeDataSet(newName)
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)  
            tkfocus(.commander)
            }
        else {
            x <- .variables[as.numeric(tkcurselection(variablesBox)) + 1]
            if (0 > length(x)) {
                tkmessageBox(message="No variables were selected.", 
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                filterNA()
                return()
                }
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            x <- paste('"', x, '"', sep="")
            command <- paste(newName, " <- na.omit(", .activeDataSet, "[,c(", paste(x, collapse=","),
                ')])', sep="")
            logger(command)
            justDoIt(command)
            activeDataSet(newName)
            tkfocus(.commander)
            }
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)  
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help("na.omit")
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)    
    tkgrid(tklabel(allVariablesFrame, text="Include all variables"), 
        allVariablesCheckBox, sticky="w")
    tkgrid(allVariablesFrame, sticky="w")
    tkgrid(tklabel(top, text="   OR", fg="red"), sticky="w")
    tkgrid(tklabel(variablesFrame, text="Variables (select one or more)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(tklabel(dataSetNameFrame, text="Name for new data set"), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)  # causes problems ?
    tkfocus(top)
    tkwait.window(top)
    }

subsetDataSet <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Subset Data Set")
    allVariablesFrame <- tkframe(top)
    allVariables <- tclVar("1")
    allVariablesCheckBox <- tkcheckbutton(allVariablesFrame, variable=allVariables)
    variablesFrame <- tkframe(top)
    variablesBox <- tklistbox(variablesFrame, height=min(4, length(.variables)),
        selectmode="multiple", background="white", exportselection="FALSE")
    variablesScroll <- tkscrollbar(variablesFrame, repeatinterval=5, command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (var in .variables) tkinsert(variablesBox, "end", var)
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
        if (newName == "<same as active data set>") newName <- .activeDataSet
        if (!is.valid.name(newName)){
            tkmessageBox(message=paste('"', newName, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            subsetDataSet()
            return()
            }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                subsetDataSet()
                return()
                }
            }
        selectVars <- if (tclvalue(allVariables) == "1") ""
            else {
                x <- .variables[as.numeric(tkcurselection(variablesBox)) + 1]
                if (0 > length(x)) {
                    tkmessageBox(message="No variables were selected.", 
                        icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(top)
                    tkdestroy(top)
                    subsetDataSet()
                    return()
                    }
                paste(", select=c(", paste(x, collapse=","), ")", sep="")
                }
        cases <- tclvalue(subsetVariable)
        selectCases <- if (cases == "<all cases>") ""
            else paste(", subset=", cases, sep="")
        if (selectVars == "" && selectCases ==""){
            tkmessageBox(message="New data set same as active data set.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            subsetDataSet()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste(newName, " <- subset(", .activeDataSet, selectCases, selectVars, ")",
            sep="")
        logger(command)
        justDoIt(command)
        activeDataSet(newName)
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)  
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help("subset")
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)    
    tkgrid(tklabel(allVariablesFrame, text="Include all variables"), 
        allVariablesCheckBox, sticky="w")
    tkgrid(allVariablesFrame, sticky="w")
    tkgrid(tklabel(top, text="   OR", fg="red"), sticky="w")
    tkgrid(tklabel(variablesFrame, text="Variables (select one or more)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(tklabel(dataSetNameFrame, text="Name for new data set"), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)  # causes problems ?
    tkfocus(top)
    tkwait.window(top)
    }

setCaseNames <- function(){
    dataSet <- activeDataSet()
    if (dataSet == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Set Case Names")
    variablesListFrame <- tkframe(top)
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="single", background="white")
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    onOK <- function(){
        variable <- as.character(tkget(variablesBox, "active"))
        var <- eval(parse(text=paste(dataSet, "$", variable, sep="")), envir=.GlobalEnv)
        if (length(var) != length(unique(var))){
            tkmessageBox(message="Case names must be unique.", icon="error", type="ok", default="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            setCaseNames()
            return()
            }
        command <- paste("row.names(", dataSet, ") <- as.character(", dataSet, "$", variable, ")", sep="")
        justDoIt(command)
        logger(command)
#        eval(parse(text=paste(dataSet, "$", variable, "<<- NULL", sep="")), envir=.GlobalEnv)
        eval(parse(text=paste(dataSet, "$", variable, "<- NULL", sep="")), envir=.GlobalEnv)
        logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
        activeDataSet(dataSet)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12",command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)    
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(row.names)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)       
    tkgrid(tklabel(top, text="Select variable containing row names"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesListFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)   
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(variablesBox, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
    
renameVariables <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Rename Variables")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.variables)),
        selectmode="multiple", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .variables) tkinsert(variableBox, "end", var)
    onOK <- function(){
        which.variables <- as.numeric(tkcurselection(variableBox)) + 1
        variables <- .variables[which.variables]
        nvariables <- length(variables)
        if (nvariables < 1) {
            tkmessageBox(message="No variables selected.", icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top) 
            renameVariables()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Variable Names")
        newnames <- rep("", nvariables)
        onOKsub <- function() {
            for (i in 1:nvariables){
                newnames[i] <- eval(parse(text=paste("tclvalue(newName", i, ")", sep="")))
                }
            if (any(newnames == "")){
                tkmessageBox(message="A variable name is empty.",
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                renameVariables()
                return()
                }
            test.names <- newnames == make.names(newnames)
            if (!all(test.names)){
                tkmessageBox(message=paste("The following variable names are not legal:\n",
                    paste(newnames[!test.names], collapse=", ")), icon="error", type="ok")
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                renameVariables()
                return()
                }                
            all.names <- eval(parse(text=paste("names(", .activeDataSet, ")")))
            all.names[which.variables] <- newnames
            if (length(unique(all.names)) != length(all.names)){
                tkmessageBox(message="Variable names are not unique",
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                renameVariables()
                return()
                }
            command <- paste("names(", .activeDataSet, ")[c(", paste(which.variables, collapse=","),
                ")] <- c(", paste('"', newnames, '"', collapse=",", sep=""), ")", sep="")
            justDoIt(command)
            logger(command)
            activeDataSet(.activeDataSet)
            if (.grab.focus) tkgrab.release(subdialog)
            tkfocus(.commander)
            tkdestroy(subdialog)
            }
        onCancelSub <- function() {
            if (.grab.focus) tkgrab.release(subdialog)  
            tkfocus(.commander)
            tkdestroy(subdialog)
            }
        OKSubButton <- tkbutton(subdialog, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subdialog, text="Cancel", fg="red", width="12",command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Old Name", fg="blue"), 
            tklabel(subdialog, text="New name", fg="blue"), sticky="w")
        for (i in 1:nvariables){
            valVar <- paste("newName", i, sep="")
            assign(valVar, tclVar(""))
            assign(paste("entry", i, sep=""), tkentry(subdialog, width="20", 
                textvariable=eval(parse(text=valVar))))
            tkgrid(tklabel(subdialog, text=variables[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
            }
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        for (row in 0:(nvariables + 1)) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:1) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)           
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(entry1)
        tkwait.window(subdialog)
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
        help(names)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick one or more)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="      "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)   # causes problems ?
    tkfocus(top)
    tkwait.window(top)
    }

setContrasts <- function(){
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
    tkwm.title(top, "Set Contrasts for Factor")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .factors) tkinsert(variableBox, "end", var)
    contrastsFrame <- tkframe(top)
    contrastsVariable <- tclVar(getOption("contrasts")[1])
    treatmentButton <- tkradiobutton(contrastsFrame, variable=contrastsVariable, value="contr.Treatment")
    sumButton <- tkradiobutton(contrastsFrame, variable=contrastsVariable, value="contr.Sum")
    helmertButton <- tkradiobutton(contrastsFrame, variable=contrastsVariable, value="contr.helmert")
    polyButton <- tkradiobutton(contrastsFrame, variable=contrastsVariable, value="contr.poly")
    specifyButton <- tkradiobutton(contrastsFrame, variable=contrastsVariable, value="specify")
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        contrasts <- tclvalue(contrastsVariable)
        if (contrasts != "specify"){
            command <- paste("contrasts(", .activeDataSet, "$", variable, ') <- "', contrasts, '"', sep="")
            justDoIt(command)
            logger(command)
            activeDataSet(.activeDataSet)          
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            }
        else{
            subdialog <- tktoplevel()
            tkwm.title(subdialog, "Specify Contrasts")
            tkgrid(tklabel(subdialog, text="Enter Contrast Coefficients", fg="blue"), sticky="w")
            env <- environment()
            tableFrame <- tkframe(subdialog)
            row.names <- eval(parse(text=paste("levels(", .activeDataSet, "$", variable, ")")))
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
                tkdestroy(top)
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
                    tkmessageBox(message=paste("Number of valid entries in contrast matrix(", length(values), ")\n",
                        "not equal to number of levels (", nrows,") * number of contrasts (", ncols,").", 
                        sep=""), icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    setContrasts()
                    return()
                    }
                if (qr(matrix(values, nrows, ncols))$rank < ncols) {
                    tkmessageBox(message="Contrast matrix is not of full column rank", 
                        icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    setContrasts()
                    return()
                    }  
                contrast.names <- rep("", ncols)
                for (j in 1:ncols){
                    varname <- paste(".col.", j, sep="")
                    contrast.names[j] <- eval(parse(text=paste("tclvalue(", varname,")", sep="")))
                    }
                if (length(unique(contrast.names)) < ncols) {
                    tkmessageBox(message="Contrast names must be unique", 
                        icon="error", type="ok")
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    setContrasts()
                    return()
                    }                    
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                command <- paste("matrix(c(", paste(values, collapse=","), "), ", nrows, ", ", ncols,
                    ")", sep="")
                assign(".Contrasts", justDoIt(command), envir=.GlobalEnv)
                logger(paste(".Contrasts <- ", command, sep=""))
                command <- paste("colnames(.Contrasts) <- c(", 
                    paste("'", contrast.names, "'", sep="", collapse=", "), ")", sep="")
                justDoIt(command)
                logger(command)
                command <- paste("contrasts(", .activeDataSet, "$", variable, ") <- .Contrasts", sep="")
                justDoIt(command)
                logger(command)
                justDoIt("remove(.Contrasts, envir=.GlobalEnv)")   
                logger("remove(.Contrasts)") 
                activeDataSet(.activeDataSet)                                      
                tkfocus(.commander)
                }
            subButtonsFrame <- tkframe(subdialog)
            OKSubButton <- tkbutton(subButtonsFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
            onCancelSub <- function() {
                if (.grab.focus) tkgrab.release(subdialog)
                tkfocus(.commander)
                tkdestroy(subdialog) 
                tkdestroy(top) 
                }
            cancelSubButton <- tkbutton(subButtonsFrame, text="Cancel", fg="red", width="12",command=onCancelSub)
            onHelpSub <- function() {
                if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(subdialog)
                help("contrasts")
                }
            helpSubButton <- tkbutton(subButtonsFrame, text="Help", width="12", command=onHelpSub)
            tkgrid(tableFrame, sticky="w")
            tkgrid(tklabel(subdialog, text=""))
            tkgrid(OKSubButton, cancelSubButton, tklabel(subButtonsFrame, text="        "), helpSubButton, sticky="w")
            tkgrid(subButtonsFrame, sticky="w")
            for (row in 0:4) tkgrid.rowconfigure(subdialog, row, weight=0)
            for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
            .Tcl("update idletasks")
            tkwm.resizable(subdialog, 0, 0)
            tkbind(subdialog, "<Return>", onOK)
            if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOK)
            tkwm.deiconify(subdialog)
            if (.grab.focus) tkgrab.set(subdialog)
            tkfocus(subdialog)
            tkwait.window(subdialog)        
            }
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
        help(contrasts)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Factor (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="nw")
    tkgrid(tklabel(top, text="Contrasts", fg="blue"), sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Treatment (dummy) contrasts"), treatmentButton, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Sum (deviation) contrasts"), sumButton, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Helmert contrasts"), helmertButton, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Polynomial contrasts"), polyButton, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Other (specify)"), specifyButton, sticky="w")
    tkgrid(contrastsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top) 
    tkfocus(top)
    tkwait.window(top)
    }
