# last modified 13 June 2003 by J. Fox

# Data menu dialogs

selectActiveDataSet <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Select Data Set")
    dataSets <- listDataSets()
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
        tkgrab.release(top)
        tkdestroy(top)
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
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
    
listDataSetsInPackages <- function() doItAndPrint("data()")

Recode <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    dataSet <- activeDataSet()
    if (dataSet == FALSE) return()
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
        height="5", width="20", wrap="none")
    recodesYscroll <- tkscrollbar(recodesFrame, repeatinterval=5,
        command=function(...) tkyview(recodes, ...))
    tkconfigure(recodes, yscrollcommand=function(...) tkset(recodesYscroll, ...))
    asFactorFrame <- tkframe(top)
    asFactorVariable <- tclVar("1")
    asFactorCheckBox <- tkcheckbutton(asFactorFrame, variable=asFactorVariable)
    onOK <- function(){
        variable <- as.character(tkget(variablesBox, "active"))
        newVar <- tclvalue(newVariableName)
        asFactor <- tclvalue(asFactorVariable) == "1"
        recode.directives <- gsub("\n", "; ", tclvalue(tkget(recodes, "1.0", "end")))
        check.empty <- gsub(";", "", gsub(" ", "", recode.directives))
        if ("" == check.empty) {
            tkmessageBox(message="No recode directives specified.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            Recode()
            return()
            }
        if (0 != length(grep("'", recode.directives))) {
            tkmessageBox(message='Use only double-quotes (" ") in recode directives', 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            Recode()
            return()
            }
        if (is.element(newVar, .variables)) {
            if ("no" == tclvalue(checkReplace(newVar))){
                tkgrab.release(top)
                tkdestroy(top)
                Recode()
                return()
                }
            }
        tkgrab.release(top)
        tkdestroy(top)
        cmd <- paste("recode(", dataSet,"$",variable, ", '", recode.directives, 
            "', as.factor.result=", asFactor, ")", sep="")
        logger(paste(dataSet,"$",newVar, " <- ", cmd, sep=""))
        justDoIt(paste(dataSet,"$",newVar, " <<- ", cmd, sep=""))
        activeDataSet(dataSet)
        tkfocus(.commander)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKCancelFrame <- tkframe(top)
    OKbutton <- tkbutton(OKCancelFrame, text="OK", width="12",command=onOK)
    cancelButton <- tkbutton(OKCancelFrame, text="Cancel", width="12",command=onCancel)    
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(recode)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)       
    tkgrid(tklabel(top, text="Variable to recode (pick one)"), 
        tklabel(top, text="Recode directives"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesListFrame, sticky="nw")
    tkgrid(tklabel(variablesFrame, text="New variable name"), sticky="w")
    tkgrid(newVariable, sticky="w")
    tkgrid(recodes, recodesYscroll, sticky="nw")
    tkgrid(variablesFrame, recodesFrame, sticky="nw")
    tkgrid(tklabel(asFactorFrame, text="Make new variable a factor"), asFactorCheckBox, 
        sticky="w")
    tkgrid(asFactorFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w") 
    tkgrid(OKCancelFrame, helpButton, sticky="w")
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid.configure(recodesYscroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(recodes)
    tkwait.window(top)             
    }

Compute <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    dataSet <- activeDataSet()
    if (dataSet == FALSE) return()
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
        newVar <- tclvalue(newVariableName)
        express <- tclvalue(computeVar)
        check.empty <- gsub(";", "", gsub(" ", "", express))
        if ("" == check.empty) {
            tkmessageBox(message="No expression specified.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            Compute()
            return()
            }
        if (is.element(newVar, .variables)) {
            if ("no" == tclvalue(checkReplace(newVar))){
                tkgrab.release(top)
                tkdestroy(top)
                Compute()
                return()
                }
            }
        tkgrab.release(top)
        tkdestroy(top)
        logger(paste(dataSet,"$",newVar, " <- ", express, sep=""))
        justDoIt(paste(dataSet,"$",newVar, " <<- with(", .activeDataSet,
            " ,", express, ")"))
        activeDataSet(dataSet)
        tkfocus(.commander)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKCancelFrame <- tkframe(top)
    OKbutton <- tkbutton(OKCancelFrame, text="OK", width="12", command=onOK)
    cancelButton <- tkbutton(OKCancelFrame, text="Cancel", width="12",command=onCancel)  
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help("Arithmetic")
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
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(compute)
    tkwait.window(top)
    }

deleteVariable <- function(){
    dataSet <- activeDataSet()
    if (dataSet == FALSE) return()
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
            eval(parse(text=paste(dataSet, "$", variable, "<<- NULL", sep="")))
            logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
            }
        activeDataSet(dataSet)
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12",command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12",command=onCancel)    
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
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
    tkwm.deiconify(top)
    tkgrab.set(top)
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
    missingVariable <- tclVar("NA")
    missingEntry <- tkentry(optionsFrame, width="8", textvariable=missingVariable)    
    onOK <- function(){
        dsnameValue <- tclvalue(dsname)
        if (dsnameValue == ""){
            tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter a name for the data set."),
                icon="error", type="ok", default="ok")
                readDataSet()
                return()
                }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                tkgrab.release(top)
                tkdestroy(top)
                readDataSet()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(filetypes='{"Text Files" {".txt"}} {"All Files" {"*"}}'))
        if (file == "") {
            tkgrab.release(top)
            tkdestroy(top)
            return()
            }
        head <- tclvalue(headerVariable) == "1"
        delimiter <- tclvalue(delimiterVariable)
        del <- if (delimiter == "whitespace") ""
            else if (delimiter == "commas") ","
            else tclvalue(otherVariable)
        miss <- tclvalue(missingVariable)
        command <- paste('read.table("', file,'", header=', head, 
            ', sep="', del, '", na.strings="', miss, '", strip.white=TRUE)', sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        assign(dsnameValue, justDoIt(command), envir=.GlobalEnv)
        activeDataSet(dsnameValue)
        tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    buttonsFrame <- tkframe(top)  
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(read.table)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(optionsFrame, text="Enter name for data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Variable names in file:"), headerCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Missing data indicator:"), missingEntry, sticky="w")
    tkgrid(tklabel(delimiterFrame, text="Field Separator"), sticky="w")
    tkgrid(tklabel(delimiterFrame, text="White space"), whitespaceButton, sticky="w")
    tkgrid(tklabel(delimiterFrame, text="Commas"), commasButton, sticky="w")
    tkgrid(tklabel(delimiterFrame, text="Other"), otherButton, 
        tklabel(delimiterFrame, text="  Specify:"), otherEntry, sticky="w")
    tkgrid(delimiterFrame, sticky="w", columnspan=2)
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(helpButton, sticky="e")   
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK) 
    tkwm.deiconify(top)
    tkgrab.set(top)
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
                    tkgrab.release(top)
                    tkdestroy(top)
                    readDataFromPackage()
                    return()
                    }
                }
            save.options <- options(warn=2)
            check <- try(eval(parse(text=logger(paste("data(", dsnameValue, ")", sep="")))), 
                silent=TRUE)
            options(save.options)
            if (class(check) == "try-error"){
                tkmessageBox(message=paste("Data set", dsnameValue, "does not exist."),
                    icon="error", type="ok")
                tkgrab.release(top)
                tkdestroy(top)
                readDataFromPackage()
                return()
                }
            activeDataSet(dsnameValue)
            tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            }
        else{
            packageName <- as.character(tkget(packagesBox, "active")) 
            save.options <- options(warn=-1)
            dataSets <- data(package=parse(text=packageName))$results[,3] 
            options(save.options)
            if (length(dataSets) == 0){
                tkgrab.release(top)
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
                        tkgrab.release(subdialog)
                        tkdestroy(subdialog)
                        readDataFromPackage()
                        return()
                        }
                    }
                command <- paste("data(", dsnameValue, ", package=", packageName, ")", sep="")
                justDoIt(command)
                logger(command)
                activeDataSet(dsnameValue)                
                tkgrab.release(subdialog)
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            onCancelSub <- function() {
                tkgrab.release(subdialog)  
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            subButtonFrame <- tkframe(subdialog)
            OKSubButton <- tkbutton(subButtonFrame, text="OK", width="12", command=onOKsub, default="active")
            cancelSubButton <- tkbutton(subButtonFrame, text="Cancel", width="12",command=onCancelSub)
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
            tkselection.set(dsBox, 0)
            tkgrab.release(top)
            tkwm.deiconify(subdialog)
            tkgrab.set(subdialog)
            tkfocus(subdialog)
            tkwait.window(subdialog)
            tkdestroy(top)           
            }
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
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
    tkselection.set(packagesBox, 0)
    tkwm.deiconify(top)
    tkgrab.set(top)
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
        dsnameValue <- tclvalue(dsname)
        if (dsnameValue == ""){
            tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter the name of a data set."),
                icon="error", type="ok", default="ok")
                importSPSS()
                return()
                }     
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                tkgrab.release(top)
                tkdestroy(top)
                importSPSS()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(
            filetypes='{"SPSS save files" {".sav"}} {"SPSS portable files" {".por"}} {"All Files" {"*"}}'))
        if (file == "") {
            tkgrab.release(top)
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
        tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
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
    tkwm.deiconify(top)
    tkgrab.set(top)
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
        dsnameValue <- tclvalue(dsname)
        if (dsnameValue == ""){
            tkgrab.release(top)
            tkdestroy(top)
            tkmessageBox(message=paste("You must enter the name of a data set."),
                icon="error", type="ok", default="ok")
                importMinitab()
                return()
                }     
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue))){
                tkgrab.release(top)
                tkdestroy(top)
                importMinitab()
                return()
                }
            }
        file <- tclvalue(tkgetOpenFile(
            filetypes='{"Minitab portable files" {".mtp"}} {"All Files" {"*"}}'))
        if (file == "") {
            tkgrab.release(top)
            tkdestroy(top)
            return()
            }
        command <- paste('read.mtp("', file,'")', sep="")
        datalist <- justDoIT(command)
        lengths <- sapply(datalist, length)
        datalist <- datalist[lengths != 0]
        lengths <- lengths[lengths != 0]
        if (!all(lengths == length(datalist[[1]]))){
            tkmessageBox(message=
                paste("Minitab data set contains elements of unequal length.\nData set cannot be converted."),
                icon="error", type="ok")
            tkdestroy(top)
            return()
            }
        assign(dsnameValue, as.data.frame(datalist), envir=.GlobalEnv)
        logger(paste(dsnameValue, " <- as.data.frame(", command, ")", sep=""))
        activeDataSet(dsnameValue)
        tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12" ,command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
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
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(entryDsname)
    tkwait.window(top)
    }

numericToFactor <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) return()
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
        name <- tclvalue(factorName)
        if (name == "<same as variable>") name <- variable
        if (is.element(name, .variables)) {
            if ("no" == tclvalue(checkReplace(name))){
                tkgrab.release(top)
                tkdestroy(top)
                numericToFactor()
                return()
                }
            }
        levelsType <- tclvalue(levelsVariable)
        if (levelsType == "names"){
            values <- sort(unique(eval(parse(text=variable))))
            nvalues <- length(values)
            if (nvalues > 30) {
                tkmessageBox(message=paste("Number of levels (", nvalues, ") too large.", sep=""),
                    icon="error", type="ok")
                tkgrab.release(top)
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
                    tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    numericToFactor()
                    return()
                    }
                if (any(names == "")){
                    tkmessageBox(message="A level name is empty.",
                        icon="error", type="ok")
                    tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    numericToFactor()
                    return()
                    }
                command <- paste("factor(", .activeDataSet, "$", variable,
                    ", labels=c(", paste(paste("'", names, "'", sep=""), collapse=","), "))", sep="")
                justDoIt(paste(.activeDataSet, "$", name, " <<- ", command, sep=""))
                logger(paste(.activeDataSet,"$", name," <- ", command, sep=""))
                activeDataSet(.activeDataSet)
                tkgrab.release(subdialog)
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            onCancelSub <- function() {
                tkgrab.release(subdialog)  
                tkfocus(.commander)
                tkdestroy(subdialog)
                }
            OKSubButton <- tkbutton(subdialog, text="OK", width="12", command=onOKsub, default="active")
            cancelSubButton <- tkbutton(subdialog, text="Cancel", width="12",command=onCancelSub)
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
            tkgrab.release(top)
            tkdestroy(top)           
            tkwm.deiconify(subdialog)
            tkgrab.set(subdialog)
            tkfocus(subdialog)
            tkwait.window(subdialog)
            }
        else{
            command <- paste("as.factor(", .activeDataSet, "$", variable, ")", sep="")
            justDoIt(paste(.activeDataSet, "$", name, " <<- ", command, sep=""))
            logger(paste(.activeDataSet, "$", name," <- ", command, sep=""))
            activeDataSet(.activeDataSet)
            tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            }
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(factor)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), 
        tklabel(top, text="Factor levels"), sticky="w")
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
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

reorderFactor <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) return()
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
        name <- tclvalue(factorName)
        if (name == "<same as original>") name <- variable
        if (is.element(name, .variables)) {
            if ("no" == tclvalue(checkReplace(name))){
                tkgrab.release(top)
                tkdestroy(top)
                numericToFactor()
                return()
                }
            }
        old.levels <- eval(parse(text=paste("levels(", variable, ")", sep="")))
        nvalues <- length(old.levels)
        ordered <- tclvalue(orderedVariable)
        if (nvalues > 30) {
            tkmessageBox(message=paste("Number of levels (", nvalues, ") too large.", sep=""),
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            numericToFactor()
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
                tkgrab.release(subdialog)
                tkdestroy(subdialog)
                reorderFactor()
                return()
                }
            levels <- old.levels[order(order)]
            ordered <- if (ordered == "1") ", ordered=TRUE" else ""
            command <- paste("factor(", .activeDataSet, "$", variable,
                ", levels=c(", paste(paste("'", levels, "'", sep=""), collapse=","), ")",
                ordered, ")", sep="")
            justDoIt(paste(.activeDataSet, "$", name, " <<- ", command, sep=""))
            logger(paste(.activeDataSet,"$", name," <- ", command, sep=""))
            activeDataSet(.activeDataSet)
            tkgrab.release(subdialog)
            tkdestroy(subdialog)
            }
        onCancelSub <- function() {
            tkgrab.release(subdialog)  
            tkfocus(.commander)
            tkdestroy(subdialog)
            }
        OKSubButton <- tkbutton(subdialog, text="OK", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subdialog, text="Cancel", width="12",command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Old levels"), tklabel(subdialog, text="New order"), sticky="w")
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
        tkdestroy(top)           
        tkwm.deiconify(subdialog)
        tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
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
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

standardize <- function(X){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) return()
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
        tkgrab.release(top)
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
                    tkgrab.release(top)
                    tkdestroy(top)
                    next
                    }
                }
            justDoIt(paste(.activeDataSet, "$", Z, " <<- .Z[,", i, "]", sep=""))
            logger(paste(.activeDataSet, "$", Z, " <- .Z[,", i, "]", sep=""))
            }
        remove(.Z, envir=.GlobalEnv)   
        logger("remove(.Z)")
        activeDataSet(.activeDataSet)
        tkfocus(.commander)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(reliability)
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
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

helpDataSet <- function(){
    if (activeDataSet() == FALSE) return()
    justDoIt(paste("help(", .activeDataSet, ")", sep=""))
    logger(paste("help(", .activeDataSet, ")", sep=""))
    }
    
variablesDataSet <- function(){
    if (activeDataSet() == FALSE) return()
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
        sep <- if (delim == "tabs") "\t"
            else if (delim == "spaces") " "
            else ","
        saveFile <- tclvalue(tkgetSaveFile(filetypes='{"Text Files" {".txt"}} {"All Files" {"*"}}',
            defaultextension="txt", initialfile=paste(dsname, ".txt", sep="")))
        if (saveFile == "") {
            tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            return()
            }
        command <- paste("write.table(", dsname, ', "', saveFile, '", sep="', sep, 
            '", col.names=', col, ", row.names=", row, ", quote=", quote,
            ', na="', missing, '")', sep="")           
        justDoIt(command)
        logger(command)
        tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    buttonsFrame <- tkframe(top)  
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", default="active", command=onOK)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(write.table)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(optionsFrame, text="Write variable names:"), colnamesCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Write row names:"), rownamesCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Quotes around character values:"), quotesCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Missing values:"), missingEntry, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Field Separator"), sticky="w", columnspan=2)
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
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
