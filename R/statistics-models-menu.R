# Statistics Menu dialogs

# last modified 23 May 03 by J. Fox

    # Models menu
    
linearRegressionModel <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Model", name, "already exists.\nOverwrite model?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Linear Regression")
    variablesFrame <- tkframe(top)
    xFrame <- tkframe(variablesFrame)
    yFrame <- tkframe(variablesFrame)
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    modelName <- tclVar("RegModel")
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable,
        xscrollcommand=function(...) tkset(subsetScroll, ...))
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkyview(subsetEntry, ...))
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        y <- as.character(tkget(yBox, "active"))
        if (0 == length(x)) {
            tkmessageBox(message="No explanatory variables selected.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            linearRegressionModel()
            return()
            }
        if (is.element(y, x)) {
            tkmessageBox(message="Response and explanatory variables must be different.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            linearRegressionModel()
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (subset == "<all valid cases>"){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        tkgrab.release(top)
        tkdestroy(top)
        modelValue <- tclvalue(modelName)
        if (is.element(modelValue, listLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue))){
                tkgrab.release(top)
                tkdestroy(top)
                linearRegressionModel()
                return()
                }
            }
        activeModel(modelValue)
        command <- paste("lm(", y, "~", paste(x, collapse="+"),
            ", data=", .activeDataSet, subset, ")", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(lm)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(tklabel(variablesFrame, text="Response variable (pick one)"), 
    tklabel(variablesFrame, text="    "),
        tklabel(variablesFrame, text="Explanatory variables (pick one or more)"), sticky="w")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yFrame, tklabel(variablesFrame, text="    "), xFrame, sticky="nw")
    tkgrid(variablesFrame, sticky="w")    
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, stick="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

linearModel <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Model", name, "already exists.\nOverwrite model?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Linear Model")
    variables <- paste(.variables, ifelse(sapply(eval(parse(text=.activeDataSet)), is.factor), 
        "[factor]", ""))
    xFrame <- tkframe(top)
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    xBox <- tklistbox(xFrame, height=min(4, length(.variables)),
        selectmode="browse", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in variables) tkinsert(xBox, "end", x)
    lhsVariable <- tclVar("")
    rhsVariable <- tclVar("")
    formulaFrame <- tkframe(top)
    rhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkyview(rhsEntry, ...))
    rhsEntry <- tkentry(formulaFrame, width="50", textvariable=rhsVariable,
        xscrollcommand=function(...) tkset(rhsScroll, ...))
    lhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkyview(lhsEntry, ...))
    lhsEntry <- tkentry(formulaFrame, width="10", textvariable=lhsVariable,
        xscrollcommand=function(...) tkset(lhsScroll, ...))
    modelName <- tclVar("LinearModel")
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable,
        xscrollcommand=function(...) tkset(subsetScroll, ...))
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkyview(subsetEntry, ...))
    onOK <- function(){
        tkgrab.release(top)
        tkdestroy(top)
        modelValue <- tclvalue(modelName)
        subset <- tclvalue(subsetVariable)
        if (subset == "<all valid cases>"){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Left-hand side of model empty.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            linearModel()
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Right-hand side of model empty.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            linearModel()
            return()
            }
        if (is.element(modelValue, listLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue))){
                tkgrab.release(top)
                tkdestroy(top)
                linearModel()
                return()
                }
            }
        activeModel(modelValue)
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        command <- paste("lm(", formula,
            ", data=", .activeDataSet, subset, ")", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(lm)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(tklabel(top, text="Variables (list only)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(tklabel(formulaFrame, text="Model formula:"), sticky="w")
    tkgrid(lhsEntry, tklabel(formulaFrame, text=" ~    "), rhsEntry, sticky="w")
    tkgrid(lhsScroll, tklabel(formulaFrame, text=""), 
        rhsScroll, sticky="w")
    tkgrid(formulaFrame)
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(lhsScroll, sticky="ew")
    tkgrid.configure(rhsScroll, sticky="ew")
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

generalizedLinearModel <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Model", name, "already exists.\nOverwrite model?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Generalized Linear Model")
    variables <- paste(.variables, ifelse(sapply(eval(parse(text=.activeDataSet)), is.factor), 
        "[factor]", ""))
    xFrame <- tkframe(top)
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    xBox <- tklistbox(xFrame, height=min(4, length(.variables)),
        selectmode="browse", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in variables) tkinsert(xBox, "end", x)
    lhsVariable <- tclVar("")
    rhsVariable <- tclVar("")
    formulaFrame <- tkframe(top)
    rhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkyview(rhsEntry, ...))
    rhsEntry <- tkentry(formulaFrame, width="50", textvariable=rhsVariable,
        xscrollcommand=function(...) tkset(rhsScroll, ...))
    lhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkyview(lhsEntry, ...))
    lhsEntry <- tkentry(formulaFrame, width="10", textvariable=lhsVariable,
        xscrollcommand=function(...) tkset(lhsScroll, ...))
    modelName <- tclVar("GLM")
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    linkFamilyFrame <- tkframe(top)
    familyFrame <- tkframe(linkFamilyFrame)
    familyScroll <- tkscrollbar(familyFrame, repeatinterval=5, 
        command=function(...) tkyview(familyBox, ...))
    familyBox <- tklistbox(familyFrame, height="4", exportselection="FALSE",
        selectmode="single", background="white",
        yscrollcommand=function(...) tkset(familyScroll, ...))
    families <- c("gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian", 
        "quasibinomial", "quasipoisson")
    for (fam in families) tkinsert(familyBox, "end", fam)
    linkFrame <- tkframe(linkFamilyFrame)
    linkScroll <- tkscrollbar(linkFrame, repeatinterval=5, 
        command=function(...) tkyview(linkBox, ...))
    linkBox <- tklistbox(linkFrame, height="4", exportselection="FALSE",
        selectmode="single", background="white",
        yscrollcommand=function(...) tkset(linkScroll, ...))
    links <- c("Canonical", "identity", "inverse", "log", "logit", "probit", 
        "cloglog", "sqrt", "1/mu^2")
    for (lnk in links) tkinsert(linkBox, "end", lnk)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable,
        xscrollcommand=function(...) tkset(subsetScroll, ...))
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkyview(subsetEntry, ...))
    onOK <- function(){
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Left-hand side of model empty.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            linearModel()
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Right-hand side of model empty.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            linearModel()
            return()
            }
        modelValue <- tclvalue(modelName)
        if (is.element(modelValue, listGeneralizedLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue))){
                tkgrab.release(top)
                tkdestroy(top)
                generalizedLinearModel()
                return()
                }
            }
        activeModel(modelValue)
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        family <- families[as.numeric(tkcurselection(familyBox)) + 1]
        link <- links[as.numeric(tkcurselection(linkBox)) + 1]
        subset <- tclvalue(subsetVariable)
        if (subset == "<all valid cases>"){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        tkgrab.release(top)
        tkdestroy(top)
        if (link == "Canonical") link <- ""
        command <- paste("glm(", formula, ", family=", family, "(", link,
            "), data=", .activeDataSet, subset, ")", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(glm)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(tklabel(top, text="Variables (list only)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(tklabel(formulaFrame, text="Model formula:"), sticky="w")
    tkgrid(lhsEntry, tklabel(formulaFrame, text=" ~    "), rhsEntry, sticky="w")
    tkgrid(lhsScroll, tklabel(formulaFrame, text=""), 
        rhsScroll, sticky="w")
    tkgrid(formulaFrame)
    tkgrid(tklabel(linkFamilyFrame, text="Family"), 
        tklabel(linkFamilyFrame, text="Link function"), sticky="w")
    tkgrid(familyBox, familyScroll, sticky="nw")
    tkgrid(linkBox, linkScroll, sticky="nw")
    tkgrid(familyFrame, linkFrame, sticky="w")
    tkgrid(linkFamilyFrame, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(rhsScroll, sticky="ew")
    tkgrid.configure(lhsScroll, sticky="ew")
    tkselection.set(familyBox, 1)
    tkselection.set(linkBox, 0)
    tkgrid.configure(familyScroll, sticky="ns")
    tkgrid.configure(linkScroll, sticky="ns")
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }
