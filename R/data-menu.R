# last modified 23 May 2003 by J. Fox

# Data menu dialogs

selectActiveDataSet <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Select Data Set")
    dataSets <- listDataSets()
    dataSetsFrame <- tkframe(top)
    dataSetsScroll <- tkscrollbar(dataSetsFrame, repeatinterval=5, 
        command=function(...) tkyview(dataSetsBox, ...))
    dataSetsBox <- tklistbox(dataSetsFrame, height=min(4, length(dataSets)),
        selectmode="single", background="white",  
        yscrollcommand=function(...) tkset(dataSetsScroll, ...))
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
    tkgrid.configure(dataSetsScroll, sticky="ns")
    tkgrid(dataSetsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
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
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    tkselection.set(variablesBox, 0)
    newVariableName <- tclVar("variable")
    newVariable <- tkentry(variablesFrame, width="20", textvariable=newVariableName)
    recodesFrame <- tkframe(top)
    recodesYscroll <- tkscrollbar(recodesFrame, repeatinterval=5,
        command=function(...) tkyview(recodes, ...))
    recodes <- tktext(recodesFrame, bg="white", font=tkfont.create(family="courier", size=10), 
        height="5", width="20",
        yscrollcommand=function(...) tkset(recodesYscroll, ...),
        wrap="none")
    onOK <- function(){
        variable <- as.character(tkget(variablesBox, "active"))
        newVar <- tclvalue(newVariableName)
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
            "', as.factor.result=TRUE)", sep="")
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
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid(variablesListFrame, sticky="nw")
    tkgrid(tklabel(variablesFrame, text="New variable name"), sticky="w")
    tkgrid(newVariable, sticky="w")
    tkgrid(recodes, recodesYscroll, sticky="nw")
    tkgrid.configure(recodesYscroll, sticky="ns")
    tkgrid(variablesFrame, recodesFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w") 
    tkgrid(OKCancelFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkfocus(top)
    tkgrab(top)               
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
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="browse", background="white",
        yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    newVariableName <- tclVar("variable")
    newVariable <- tkentry(variablesFrame, width="20", textvariable=newVariableName)
    computeFrame <- tkframe(top)
    computeXscroll <- tkscrollbar(computeFrame, repeatinterval=10,
        orient="horizontal", command=function(...) tkyview(compute, ...))
    computeVar <- tclVar("")
    compute <- tkentry(computeFrame, font=.logFont, width="30", textvariable=computeVar,
        xscrollcommand=function(...) tkset(computeXscroll, ...))
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
        logger(paste("# Warning: Re-executing the following line\n",
            "#  in the R Console requires that variables\n",
            "#  in the data frame ", dataSet, " are not masked.\n",
            dataSet,"$",newVar, " <- ", express, sep=""))
        justDoIt(paste(dataSet,"$",newVar, " <<- ", express, sep=""))
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
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid(variablesListFrame, sticky="w")
    tkgrid(tklabel(variablesFrame, text="New variable name"), sticky="w")
    tkgrid(newVariable, sticky="w")
    tkgrid(tklabel(computeFrame, text="Expression to compute"), sticky="w")
    tkgrid(compute, sticky="w")
    tkgrid(computeXscroll, sticky="ew")
    tkgrid(variablesFrame, computeFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w") 
    tkgrid(OKCancelFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")    
    tkfocus(top)
    tkgrab(top)               
    }

deleteVariable <- function(){
    dataSet <- activeDataSet()
    if (dataSet == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Delete Variable")
    variablesListFrame <- tkframe(top)
    variablesScroll <- tkscrollbar(variablesListFrame, repeatinterval=5, 
        command=function(...) tkyview(variablesBox, ...))
    variablesBox <- tklistbox(variablesListFrame, height=min(4, length(.variables)),
        selectmode="single", background="white",
        yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .variables) tkinsert(variablesBox, "end", variable)
    onOK <- function(){
        variable <- as.character(tkget(variablesBox, "active"))
        response <- tclvalue(tkmessageBox(message=paste("Delete ", variable,
            "?\nPlease confirm.", sep=""), icon="warning", type="okcancel", default="cancel"))
        if (response == "cancel") {
            onCancel()
            return()
            }
        eval(parse(text=paste(dataSet, "$", variable, "<<- NULL", sep="")))
        logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
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
    tkgrid(tklabel(top, text="Variable to delete"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid(variablesListFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    }

readDataSet <- function() {
    checkReplace <- function(name){
        tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
            icon="warning", type="yesno", default="no")
        }
    top <- tktoplevel()
    tkwm.title(top, "Read Data From Text File")
    dsname <- tclVar("Dataset")
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    header <- tclVar("1")
    headerCheckBox <- tkcheckbutton(top, variable=header)
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
        head <- tclvalue(header) == "1"
        logger(paste(dsnameValue, ' <- read.table("', file,'", header=', head, ")", sep=""))
        assign(dsnameValue, read.table(file, header=head), envir=.GlobalEnv)
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
    tkbind(entryDsname, "<Return>", onOK)
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(top, text="Variable names in file:"), headerCheckBox, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w", columnspan="2")
    tkgrid.configure(helpButton, sticky="e")    
    tkfocus(top)
    tkgrab(top)
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
    packagesScroll <- tkscrollbar(packagesFrame, repeatinterval=5, 
        command=function(...) tkyview(packagesBox, ...))
    packagesBox <- tklistbox(packagesFrame, height=min(4, length(packages)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(packagesScroll, ...))
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
            dsScroll <- tkscrollbar(dsFrame, repeatinterval=5, 
                command=function(...) tkyview(dsBox, ...))
            dsBox <- tklistbox(dsFrame, height=min(4, length(dataSets)),
                selectmode="single", background="white", exportselection="FALSE",
                yscrollcommand=function(...) tkset(dsScroll, ...))
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
                command <- paste("data(", dsnameValue, ")", sep="")
                justDoIt(command)
                logger(command)
                activeDataSet(dsnameValue)                
                tkgrab.release(subdialog)
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
            tkgrid.configure(dsScroll, sticky="ns")
            tkgrid(tklabel(labelFrame, text="Package: "), tklabel(labelFrame, text=packageName, fg="red"),
                sticky="w")
            tkgrid(labelFrame, sticky="w")
            tkgrid(tklabel(subdialog, text="Select data set"), sticky="w")
            tkgrid(dsFrame, sticky="w")
            tkgrid(OKSubButton, cancelSubButton, sticky="w")
            tkgrid(subButtonFrame, sticky="w")
            tkbind(subdialog, "<Return>", onOKsub)
            tkselection.set(dsBox, 0)
            tkgrab.release(top)
            tkfocus(subdialog)
            tkgrab(subdialog)
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
    tkbind(entryDsname, "<Return>", onOK)
    tkgrid(packagesBox, packagesScroll, sticky="nw")
    tkgrid.configure(packagesScroll, sticky="ns")
    tkgrid(tklabel(top, text="Enter name of data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(top, text="OR", fg="red"), sticky="w")
    tkgrid(tklabel(top, text="Select package:"), packagesFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(packagesBox, 0)
    tkfocus(top)
    tkgrab(top)
    tkfocus(entryDsname)
    }


##  readDataFromPackage <- function() {
##      checkReplace <- function(name){
##          tkmessageBox(message=paste("Data set", name, "already exists.\nOverwrite data set?"),
##              icon="warning", type="yesno", default="no")
##          }
##      top <- tktoplevel()
##      tkwm.title(top, "Read Data From Package")
##      dsname <- tclVar("")
##      entryDsname <- tkentry(top, width="20", textvariable=dsname)
##      onOK <- function(){
##          dsnameValue <- tclvalue(dsname)
##          if (dsnameValue == ""){
##              tkgrab.release(top)
##              tkdestroy(top)
##              tkmessageBox(message=paste("You must enter the name of a data set."),
##                  icon="error", type="ok", default="ok")
##                  readDataFromPackage()
##                  return()
##                  }        
##          if (is.element(dsnameValue, listDataSets())) {
##              if ("no" == tclvalue(checkReplace(dsnameValue))){
##                  tkgrab.release(top)
##                  tkdestroy(top)
##                  readDataFromPackage()
##                  return()
##                  }
##              }
##          save.options <- options(warn=2)
##          check <- try(eval(parse(text=logger(paste("data(", dsnameValue, ")", sep="")))), 
##              silent=TRUE)
##          options(save.options)
##          if (class(check) == "try-error"){
##              tkmessageBox(message=paste("Data set", dsnameValue, "does not exist."),
##                  icon="error", type="ok")
##              tkgrab.release(top)
##              tkdestroy(top)
##              readDataFromPackage()
##              return()
##              }
##          activeDataSet(dsnameValue)
##          tkgrab.release(top)
##          tkdestroy(top)
##          tkfocus(.commander)
##          }
##      onCancel <- function() {
##          tkgrab.release(top)
##          tkfocus(.commander)
##          tkdestroy(top)  
##          } 
##      buttonsFrame <- tkframe(top)
##      OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", default="active", command=onOK)
##      cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12",command=onCancel)
##      onHelp <- function() {
##          if (.Platform$OS.type != "windows") tkgrab.release(top)
##          help(data)
##          }
##      helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
##      tkbind(entryDsname, "<Return>", onOK)
##      tkgrid(tklabel(top, text="Enter name of data set:"), entryDsname, sticky="w")
##      tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
##      tkgrid(buttonsFrame, columnspan="2", sticky="w")
##      tkgrid.configure(helpButton, sticky="e")
##      tkfocus(top)
##      tkgrab(top)
##      tkfocus(entryDsname)
##      }

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
                readDataSet()
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
    tkbind(entryDsname, "<Return>", onOK)
    tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
    tkgrid(tklabel(top, text="Convert value labels\nto factor levels"), 
        asFactorCheckBox, sticky="e")
    tkgrid(tklabel(top, text="Maximum number\nof value labels\nfor factor conversion"), 
        entryMaxLevels, sticky="e")
    tkgrid.configure(entryDsname, sticky="w")
    tkgrid.configure(asFactorCheckBox, sticky="w")
    tkgrid.configure(entryMaxLevels, sticky="w")
    tkbind(entryDsname, "<Return>", onOK)
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkfocus(top)
    tkgrab(top)    
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
                readDataSet()
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
    tkgrid.configure(entryDsname, sticky="w")
    tkbind(entryDsname, "<Return>", onOK)
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(helpButton, sticky="e")    
    tkfocus(top)
    tkgrab(top)
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
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(variableScroll, ...))
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
            tkbind(subdialog, "<Return>", onOKsub)
            tkfocus(subdialog)
            tkgrab(subdialog)
            tkdestroy(top)           
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
    tkgrid.configure(variableScroll, sticky="ns")
    tkgrid.configure(numbersButton, sticky="w")
    tkgrid.configure(namesButton, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
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
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(variableScroll, ...))
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
        tkbind(subdialog, "<Return>", onOKsub)
        tkfocus(subdialog)
        tkgrab(subdialog)
        tkdestroy(top)           
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
    tkgrid(tklabel(top, text="Factor (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="nw")
    tkgrid(tklabel(top, text="Name for factor"), sticky="w")
    tkgrid(factorNameField, sticky="w")
    tkgrid(tklabel(orderedFrame, text="Make ordered factor"), orderedCheckBox, sticky="w")
    tkgrid(orderedFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
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
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(xScroll, ...))
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
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variables (pick one or more)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
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
