Recode <- function(){
    require("car")
    processRecode <- function(recode){
        parts <- strsplit(recode, "=")[[1]]
        if (length(grep(",", parts[1])) > 0) paste("c(", parts[1], ") = ", parts[2], sep="")
            else paste(parts, collapse="=")
        }
    dataSet <- activeDataSet()
    initializeDialog(title=gettextRcmdr("Recode Variables"))
    variablesBox <- variableListBox(top, Variables(), 
        selectmode="multiple", title=gettextRcmdr("Variables to recode (pick one or more)"))
    variablesFrame <- tkframe(top)
    newVariableName <- tclVar(gettextRcmdr("variable"))
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
        asFactor <- tclvalue(asFactorVariable) == "1"
        recode.directives <- gsub("\n", "; ", tclvalue(tkget(recodes, "1.0", "end")))
        check.empty <- gsub(";", "", gsub(" ", "", recode.directives))
        if ("" == check.empty) {
            errorCondition(recall=Recode,
                message=gettextRcmdr("No recode directives specified."))
            return()
            }
        if (0 != length(grep("'", recode.directives))) {
            errorCondition(recall=Recode,
                message=gettextRcmdr('Use only double-quotes (" ") in recode directives'))
            return()
            }
        recode.directives <- strsplit(recode.directives, ";")[[1]]
        recode.directives <- paste(sapply(recode.directives, processRecode), collapse=";") 
        variables <- getSelection(variablesBox)
        closeDialog()
        if (length(variables) == 0) {
            errorCondition(recall=Recode, message=gettextRcmdr("You must select a variable."))
            return()
            }
        multiple <- if (length(variables) > 1) TRUE else FALSE
        name <- trim.blanks(tclvalue(newVariableName))
        for (variable in variables){
            newVar <- if (multiple) paste(name, variable, sep="") else name
            if (!is.valid.name(newVar)){
                errorCondition(recall=Recode,
                    message=paste('"', newVar, '" ', 
                        gettextRcmdr("is not a valid name."), sep=""))
                return()
                }
            if (is.element(newVar, Variables())) {
                if ("no" == tclvalue(checkReplace(newVar))){
                    Recode()
                    return()
                    }
                }
            cmd <- paste("recode(", dataSet,"$",variable, ", '", recode.directives, 
                "', as.factor.result=", asFactor, ")", sep="")
            logger(paste(dataSet,"$",newVar, " <- ", cmd, sep=""))
            justDoIt(paste(dataSet,"$",newVar, " <- ", cmd, sep=""))
            activeDataSet(dataSet, flushModel=FALSE)
            tkfocus(CommanderWindow())
            }
        }
    OKCancelHelp(helpSubject="Recode")    
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(tklabel(variablesFrame, text=""))
    tkgrid(tklabel(variablesFrame, 
        text=gettextRcmdr("New variable name or prefix for multiple recodes: ")),
        newVariable, sticky="w")
    tkgrid(tklabel(asFactorFrame, 
        text=gettextRcmdr("Make (each) new variable a factor")), asFactorCheckBox, 
        sticky="w")
    tkgrid(tklabel(asFactorFrame, text=""))
    tkgrid(tklabel(recodesFrame, text=gettextRcmdr("Enter recode directives"), fg="blue"), 
        sticky="w")
    tkgrid(recodes, recodesYscroll, sticky="nw")
    tkgrid(recodesXscroll)
    tkgrid(variablesFrame, sticky="w")
    tkgrid(asFactorFrame, sticky="w")
    tkgrid(recodesFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(recodesXscroll, sticky="ew")
    tkgrid.configure(recodesYscroll, sticky="ns")
    dialogSuffix(rows=4, columns=2, bindReturn=FALSE)        
    }