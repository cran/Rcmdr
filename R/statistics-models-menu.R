# Statistics Menu dialogs

# last modified 23 July 03 by J. Fox

    # Models menu
    
linearRegressionModel <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Model", name, "already exists.\nOverwrite model?"),
            icon="warning", type="yesno", default="no")
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 2){
        tkmessageBox(message="There are fewer than 2 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Linear Regression")
    variablesFrame <- tkframe(top)
    xFrame <- tkframe(variablesFrame)
    yFrame <- tkframe(variablesFrame)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    tkconfigure(yBox, yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    assign(".modelNumber", .modelNumber + 1, envir=.GlobalEnv)
    modelName <- tclVar(paste("RegModel.", .modelNumber, sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        y <- as.character(tkget(yBox, "active"))
        if (0 == length(x)) {
            tkmessageBox(message="No explanatory variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearRegressionModel()
            return()
            }
        if (is.element(y, x)) {
            tkmessageBox(message="Response and explanatory variables must be different.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearRegressionModel()
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == "<all valid cases>"){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            tkmessageBox(message=paste('"', modelValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearRegressionModel()
            return()
            }
        if (is.element(modelValue, listLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue))){
                if (.grab.focus) tkgrab.release(top)
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
        assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
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
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

linearModel <- function(){
    formulaFields <- function(model){
        formula <- as.character(model$call$formula)
        lhs <- formula[2]
        rhs <- formula[3]
        data <- as.character(model$call$data)
        which.subset <- which("subset" == names(model$call))
        subset <- if (0 == length(which.subset)) ""
        else as.character(model$call)[[which.subset]]
        list(lhs=lhs, rhs=rhs, data=data, subset=subset)
        }
    checkReplace <- function(name){
        tkmessageBox(message=paste("Model", name, "already exists.\nOverwrite model?"),
            icon="warning", type="yesno", default="no")
        }
    checkAddOperator <- function(rhs){
        rhs.chars <- rev(strsplit(rhs, "")[[1]])
        if (length(rhs.chars) < 1) return(FALSE)
        check.char <- if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                rhs.chars[1] else rhs.chars[2]
        !is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%"))
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
    if (length(.variables) < 2){
        tkmessageBox(message="There fewer than 2 variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Linear Model")
    variables <- paste(.variables, ifelse(sapply(eval(parse(text=.activeDataSet), 
        envir=.GlobalEnv), is.factor), "[factor]", ""))
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.variables)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in variables) tkinsert(xBox, "end", x)
    currentModel <- if (!is.null(.activeModel)) 
        eval(parse(text=paste("class(", .activeModel, ")[1] == 'lm'", sep="")), 
            envir=.GlobalEnv) 
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(eval(parse(text=.activeModel), 
            envir=.GlobalEnv))
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    lhsVariable <- if (currentModel) tclVar(currentFields$lhs) else tclVar("")
    rhsVariable <- if (currentModel) tclVar(currentFields$rhs) else tclVar("")
    formulaFrame <- tkframe(top)
    lhsEntry <- tkentry(formulaFrame, width="10", textvariable=lhsVariable)
    lhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkxview(lhsEntry, ...))
    tkconfigure(lhsEntry, xscrollcommand=function(...) tkset(lhsScroll, ...))
    rhsEntry <- tkentry(formulaFrame, width="50", textvariable=rhsVariable)
    rhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkxview(rhsEntry, ...))
    tkconfigure(rhsEntry, xscrollcommand=function(...) tkset(rhsScroll, ...))
    assign(".modelNumber", .modelNumber + 1, envir=.GlobalEnv)
    modelName <- tclVar(paste("LinearModel.", .modelNumber, sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    subsetVariable <- if (currentModel && currentFields$subset != "") 
        tclVar(currentFields$subset) else tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    onDoubleClick <- function(){
        var <- as.character(tkget(xBox, "active"))[1]
        lhs <- tclvalue(lhsVariable)
        if (lhs == "") tclvalue(lhsVariable) <- var
        else {
            tkfocus(rhsEntry)
            rhs <- tclvalue(rhsVariable)
            rhs.chars <- rev(strsplit(rhs, "")[[1]])
            check.char <- if (length(rhs.chars) > 0){
                if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                    rhs.chars[1] else rhs.chars[2]
                }
                else ""
            tclvalue(rhsVariable) <- if (rhs == "" || 
                is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%")))
                    paste(rhs, var, sep="")
                else paste(rhs, "+", var)
            }
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPlus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "+ ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onTimes <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "*", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onColon <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ":", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onSlash <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "/",  sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onIn <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "%in% ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onMinus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "- ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPower <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "^", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onLeftParen <- function(){
        tkfocus(rhsEntry)
        rhs <- tclvalue(rhsVariable)
        tclvalue(rhsVariable) <- paste(rhs, "(", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onRightParen <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ")", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onOK <- function(){
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            tkmessageBox(message=paste('"', modelValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearModel()
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == "<all valid cases>"){
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
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearModel()
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Right-hand side of model empty.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linearModel()
            return()
            }
        if (is.element(modelValue, listLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue))){
                if (.grab.focus) tkgrab.release(top)
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
        assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    operatorsFrame <- tkframe(formulaFrame)
    plusButton <- tkbutton(operatorsFrame, text="+", width="3", command=onPlus, 
        font=.operatorFont)
    timesButton <- tkbutton(operatorsFrame, text="*", width="3", command=onTimes, 
        font=.operatorFont)
    colonButton <- tkbutton(operatorsFrame, text=":", width="3", command=onColon, 
        font=.operatorFont)
    slashButton <- tkbutton(operatorsFrame, text="/", width="3", command=onSlash, 
        font=.operatorFont)
    inButton <- tkbutton(operatorsFrame, text="%in%", width="3", command=onIn,
        font=.operatorFont)
    minusButton <- tkbutton(operatorsFrame, text="-", width="3", command=onMinus, 
        font=.operatorFont)
    powerButton <- tkbutton(operatorsFrame, text="^", width="3", command=onPower, 
        font=.operatorFont)
    leftParenButton <- tkbutton(operatorsFrame, text="(", width="3", command=onLeftParen, 
        font=.operatorFont)
    rightParenButton <- tkbutton(operatorsFrame, text=")", width="3", command=onRightParen, 
        font=.operatorFont)
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(linearModel)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(tklabel(top, text="Variables (double-click to formula)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(plusButton, timesButton, colonButton, slashButton, inButton, minusButton,
        powerButton, leftParenButton, rightParenButton, sticky="w")
    tkgrid(tklabel(formulaFrame, text="Model formula:"), tklabel(formulaFrame, text=""),
        operatorsFrame, sticky="sw")
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
    for (row in 0:6) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    tkbind(xBox, "<Double-ButtonPress-1>", onDoubleClick)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(lhsEntry)
    tkwait.window(top)
    }

generalizedLinearModel <- function(){
    formulaFields <- function(model){
        formula <- as.character(model$call$formula)
        lhs <- formula[2]
        rhs <- formula[3]
        data <- as.character(model$call$data)
        fam <- as.character(model$call$family)
        family <- fam[1]
        link <- fam[2]
        which.subset <- which("subset" == names(model$call))
        subset <- if (0 == length(which.subset)) ""
        else as.character(model$call)[[which.subset]]
        list(lhs=lhs, rhs=rhs, data=data, subset=subset, family=family, link=link)
        }
    checkReplace <- function(name){
        tkmessageBox(message=paste("Model", name, "already exists.\nOverwrite model?"),
            icon="warning", type="yesno", default="no")
        }
    checkAddOperator <- function(rhs){
        rhs.chars <- rev(strsplit(rhs, "")[[1]])
        if (length(rhs.chars) < 1) return(FALSE)
        check.char <- if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                rhs.chars[1] else rhs.chars[2]
        !is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%"))
        }
    families <- c("gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian", 
        "quasibinomial", "quasipoisson")
    links <- c("identity", "inverse", "log", "logit", "probit", 
        "cloglog", "sqrt", "1/mu^2")  
    availableLinks <- matrix(c(
        TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
        TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE,
        TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
        TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,
        FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
        TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE),
        7, 8, byrow=TRUE)
    rownames(availableLinks) <- families
    colnames(availableLinks) <- links
    canonicalLinks <- c("identity", "logit", "log", "inverse", "1/mu^2", "logit", "log")
    names(canonicalLinks) <- families
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.variables) < 2){
        tkmessageBox(message="There fewer than 2 variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Generalized Linear Model")
    variables <- paste(.variables, ifelse(sapply(eval(parse(text=.activeDataSet), 
        envir=.GlobalEnv), is.factor), "[factor]", ""))
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.variables)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in variables) tkinsert(xBox, "end", x)
    currentModel <- if (!is.null(.activeModel)) 
        eval(parse(text=paste("class(", .activeModel, ")[1] == 'glm'", sep="")), 
            envir=.GlobalEnv)
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(eval(parse(text=.activeModel), 
            envir=.GlobalEnv))
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    lhsVariable <- if (currentModel) tclVar(currentFields$lhs) else tclVar("")
    rhsVariable <- if (currentModel) tclVar(currentFields$rhs) else tclVar("")
    formulaFrame <- tkframe(top)
    lhsEntry <- tkentry(formulaFrame, width="10", textvariable=lhsVariable)
    lhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkxview(lhsEntry, ...))
    tkconfigure(lhsEntry, xscrollcommand=function(...) tkset(lhsScroll, ...))
    rhsEntry <- tkentry(formulaFrame, width="50", textvariable=rhsVariable)
    rhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
        orient="horizontal", command=function(...) tkxview(rhsEntry, ...))
    tkconfigure(rhsEntry, xscrollcommand=function(...) tkset(rhsScroll, ...))
    assign(".modelNumber", .modelNumber + 1, envir=.GlobalEnv)
    modelName <- tclVar(paste("GLM.", .modelNumber, sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    linkFamilyFrame <- tkframe(top)
    familyFrame <- tkframe(linkFamilyFrame)
    familyBox <- tklistbox(familyFrame, height="4", exportselection="FALSE",
        selectmode="single", background="white")
    familyScroll <- tkscrollbar(familyFrame, repeatinterval=5, 
        command=function(...) tkyview(familyBox, ...))
    tkconfigure(familyBox, yscrollcommand=function(...) tkset(familyScroll, ...))
    for (fam in families) tkinsert(familyBox, "end", fam)
    linkFrame <- tkframe(linkFamilyFrame)
    linkBox <- tklistbox(linkFrame, height="4", exportselection="FALSE",
        selectmode="single", background="white")
    subsetVariable <- if (currentModel && currentFields$subset != "") 
        tclVar(currentFields$subset) else tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    onDoubleClick <- function(){
        var <- as.character(tkget(xBox, "active"))[1]
        lhs <- tclvalue(lhsVariable)
        if (lhs == "") tclvalue(lhsVariable) <- var
        else {
            tkfocus(rhsEntry)
            rhs <- tclvalue(rhsVariable)
            rhs.chars <- rev(strsplit(rhs, "")[[1]])
            check.char <- if (length(rhs.chars) > 0){
                if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                    rhs.chars[1] else rhs.chars[2]
                }
                else ""
            tclvalue(rhsVariable) <- if (rhs == "" || 
                is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%")))
                    paste(rhs, var, sep="")
                else paste(rhs, "+", var)
            }
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPlus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "+ ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onTimes <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "*", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onColon <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ":", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onSlash <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "/",  sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onIn <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "%in% ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onMinus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "- ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPower <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "^", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onLeftParen <- function(){
        tkfocus(rhsEntry)
        rhs <- tclvalue(rhsVariable)
        tclvalue(rhsVariable) <- paste(rhs, "(", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onRightParen <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ")", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onFamilySelect <- function(){
        family <- families[as.numeric(tkcurselection(familyBox)) + 1]
        availLinks <- links[availableLinks[family,]]
        tkdelete(linkBox, "0", "end")
        for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
        canLink <- canonicalLinks[family]
        tkconfigure(linkBox, height=length(availLinks))
        tkselection.set(linkBox, which(canLink == availLinks) - 1)
        }
    onOK <- function(){
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Left-hand side of model empty.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            generalizedLinearModel()
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            tkmessageBox(message="Right-hand side of model empty.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            generalizedLinearModel()
            return()
            }
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            tkmessageBox(message=paste('"', modelValue, '" is not a valid name.', 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            generalizedLinearModel()
            return()
            }
        if (is.element(modelValue, listGeneralizedLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                generalizedLinearModel()
                return()
                }
            }
        activeModel(modelValue)
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        family <- families[as.numeric(tkcurselection(familyBox)) + 1]
        availLinks <- links[availableLinks[family,]]
        link <- availLinks[as.numeric(tkcurselection(linkBox)) + 1]
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == "<all valid cases>"){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("glm(", formula, ", family=", family, "(", link,
            "), data=", .activeDataSet, subset, ")", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    operatorsFrame <- tkframe(formulaFrame)
    plusButton <- tkbutton(operatorsFrame, text="+", width="3", command=onPlus, 
        font=.operatorFont)
    timesButton <- tkbutton(operatorsFrame, text="*", width="3", command=onTimes, 
        font=.operatorFont)
    colonButton <- tkbutton(operatorsFrame, text=":", width="3", command=onColon, 
        font=.operatorFont)
    slashButton <- tkbutton(operatorsFrame, text="/", width="3", command=onSlash, 
        font=.operatorFont)
    inButton <- tkbutton(operatorsFrame, text="%in%", width="3", command=onIn,
        font=.operatorFont)
    minusButton <- tkbutton(operatorsFrame, text="-", width="3", command=onMinus, 
        font=.operatorFont)
    powerButton <- tkbutton(operatorsFrame, text="^", width="3", command=onPower, 
        font=.operatorFont)
    leftParenButton <- tkbutton(operatorsFrame, text="(", width="3", command=onLeftParen, 
        font=.operatorFont)
    rightParenButton <- tkbutton(operatorsFrame, text=")", width="3", command=onRightParen, 
        font=.operatorFont)
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(generalizedLinearModel)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(tklabel(top, text="Variables (double-click to formula)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")
    tkgrid(plusButton, timesButton, colonButton, slashButton, inButton, minusButton,
        powerButton, leftParenButton, rightParenButton, sticky="w")
    tkgrid(tklabel(formulaFrame, text="Model formula:"), tklabel(formulaFrame, text=""),
        operatorsFrame, sticky="sw")
    tkgrid(lhsEntry, tklabel(formulaFrame, text=" ~    "), rhsEntry, sticky="w")
    tkgrid(lhsScroll, tklabel(formulaFrame, text=""), 
        rhsScroll, sticky="w")
    tkgrid(formulaFrame)
    tkgrid(tklabel(linkFamilyFrame, text="Family (double-click to select)"), 
        tklabel(linkFamilyFrame, text="   "), tklabel(linkFamilyFrame, text="Link function"), sticky="w")
    tkgrid(familyBox, familyScroll, sticky="nw")
    tkgrid(linkBox, sticky="nw")
    tkgrid(familyFrame, tklabel(linkFamilyFrame, text="   "), linkFrame, sticky="nw")
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
    tkgrid.configure(familyScroll, sticky="ns")
    for (row in 0:6) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    fam <- if (currentModel) which(currentFields$family == families) - 1
        else 1
    tkselection.set(familyBox, fam)
    availLinks <- links[availableLinks[fam + 1,]]
    for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
    tkconfigure(linkBox, height=length(availLinks))
    lnk <- if (currentModel) which(currentFields$link == availLinks) - 1
            else 0
    tkselection.set(linkBox, lnk)
    tkbind(top, "<Return>", onOK)
    tkbind(xBox, "<Double-ButtonPress-1>", onDoubleClick)
    tkbind(familyBox, "<Double-ButtonPress-1>", onFamilySelect)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(lhsEntry)
    tkwait.window(top)
    }
