# Statistics Menu dialogs

# last modified 18 Nov 04 by J. Fox

    # Models menu
    
linearRegressionModel <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkNumeric(2)) return()
    initializeDialog(title="Linear Regression")
    variablesFrame <- tkframe(top)
    xBox <- variableListBox(variablesFrame, .numeric, selectmode="multiple", 
        title="Explanatory variables (pick one or more)")
    yBox <- variableListBox(variablesFrame, .numeric, title="Response variable (pick one)")
    assign(".modelNumber", .modelNumber + 1, envir=.GlobalEnv)
    modelName <- tclVar(paste("RegModel.", .modelNumber, sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    subsetBox()
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if (0 == length(y)) {
            assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv) 
            errorCondition(recall=linearRegressionModel, message="You must select a response variable.")
            return()
            }
        if (0 == length(x)) {
            assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv) 
            errorCondition(recall=linearRegressionModel, message="No explanatory variables selected.")
            return()
            }        
        if (is.element(y, x)) {
            assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv) 
            errorCondition(recall=linearRegressionModel, message="Response and explanatory variables must be different.")
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == "<all valid cases>" || trim.blanks(subset) == ""){
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
            assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv) 
            errorCondition(recall=linearRegressionModel, message=paste('"', modelValue, '" is not a valid name.', sep=""))
            return()
            }
        if (is.element(modelValue, listLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue, type="Model"))){
                assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv) 
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
    OKCancelHelp(helpSubject="lm", model=TRUE)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(getFrame(yBox), tklabel(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
    tkgrid(variablesFrame, sticky="w")    
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, stick="w")
    tkgrid.configure(helpButton, sticky="e")
    dialogSuffix(rows=4, columns=1)
    }

linearModel <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkNumeric()) return()
    if (!checkVariables(2)) return()
    initializeDialog(title="Linear Model")
    currentModel <- if (!is.null(.activeModel)) 
        eval(parse(text=paste("class(", .activeModel, ")[1] == 'lm'", sep="")), 
            envir=.GlobalEnv) 
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(eval(parse(text=.activeModel), 
            envir=.GlobalEnv))
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    assign(".modelNumber", .modelNumber + 1, envir=.GlobalEnv)
    modelName <- tclVar(paste("LinearModel.", .modelNumber, sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    onOK <- function(){
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            errorCondition(recall=linearModel, message=paste('"', modelValue, '" is not a valid name.', sep=""), model=TRUE)
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == "<all valid cases>" || trim.blanks(subset) == ""){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=linearModel, message="Left-hand side of model empty.", model=TRUE) 
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=linearModel, message="Right-hand side of model empty.", model=TRUE)
            return()
            }
        if (is.element(modelValue, listLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue, type="Model"))){
                assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv) 
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
    OKCancelHelp(helpSubject="linearModel", model=TRUE)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    modelFormula()
    subsetBox(model=TRUE)
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(outerOperatorsFrame, sticky="w")
    tkgrid(formulaFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=lhsEntry)
    }

generalizedLinearModel <- function(){
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
    if (!checkActiveDataSet()) return()
    if (!checkVariables(2)) return()
    initializeDialog(title="Generalized Linear Model")
    currentModel <- if (!is.null(.activeModel)) 
        eval(parse(text=paste("class(", .activeModel, ")[1] == 'glm'", sep="")), 
            envir=.GlobalEnv)
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(eval(parse(text=.activeModel), 
            envir=.GlobalEnv), glm=TRUE)
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    modelFormula()
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
    subsetBox(model=TRUE)
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
            errorCondition(recall=generalizedLinearModel, model=TRUE, message="Left-hand side of model empty.")
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=generalizedLinearModel, model=TRUE, message="Right-hand side of model empty.")
            return()
            }
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            errorCondition(recall=generalizedLinearModel, model=TRUE, message=paste('"', modelValue, '" is not a valid name.', sep=""))
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
        if (trim.blanks(subset) == "<all valid cases>" || trim.blanks(subset) == ""){
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
    OKCancelHelp(helpSubject="generalizedLinearModel")
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")    
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(outerOperatorsFrame, sticky="w")
    tkgrid(formulaFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
        tkgrid(tklabel(linkFamilyFrame, text="Family (double-click to select)", fg="blue"), 
        tklabel(linkFamilyFrame, text="   "), tklabel(linkFamilyFrame, text="Link function", fg="blue"), sticky="w")
    tkgrid(familyBox, familyScroll, sticky="nw")
    tkgrid(linkBox, sticky="nw")
    tkgrid(familyFrame, tklabel(linkFamilyFrame, text="   "), linkFrame, sticky="nw")
    tkgrid(linkFamilyFrame, sticky="w")    
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(familyScroll, sticky="ns")   
    fam <- if (currentModel) which(currentFields$family == families) - 1
        else 1
    tkselection.set(familyBox, fam)
    availLinks <- links[availableLinks[fam + 1,]]
    for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
    tkconfigure(linkBox, height=length(availLinks))
    lnk <- if (currentModel) which(currentFields$link == availLinks) - 1
            else 0
    tkselection.set(linkBox, lnk)
    tkbind(familyBox, "<Double-ButtonPress-1>", onFamilySelect)
    dialogSuffix(rows=7, columns=1, focus=lhsEntry)
    }

proportionalOddsModel <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkVariables(2)) return()
    initializeDialog(title="Multinomial Logit Model")
    currentModel <- if (!is.null(.activeModel)) 
        eval(parse(text=paste("class(", .activeModel, ")[1] == 'multinom'", sep="")), 
            envir=.GlobalEnv) 
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(eval(parse(text=.activeModel), 
            envir=.GlobalEnv))
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    assign(".modelNumber", .modelNumber + 1, envir=.GlobalEnv)
    modelName <- tclVar(paste("POM.", .modelNumber, sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    onOK <- function(){
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            errorCondition(recall=proportionalOddsModel, message=paste('"', modelValue, '" is not a valid name.', sep=""), model=TRUE)
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == "<all valid cases>" || trim.blanks(subset) == ""){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=proportionalOddsModel, message="Left-hand side of model empty.", model=TRUE) 
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=proportionalOddsModel, message="Right-hand side of model empty.", model=TRUE)
            return()
            }
        if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=eval(parse(text=.activeDataSet), envir=.GlobalEnv)))){
            errorCondition(recall=proportionalOddsModel, message="Response variable must be a factor")
            return()
            }
        if (is.element(modelValue, listProportionalOddsModels())) {
            if ("no" == tclvalue(checkReplace(modelValue, type="Model"))){
                assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv) 
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                linearModel()
                return()
                }
            }
        activeModel(modelValue)
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        command <- paste("polr(", formula,
            ", data=", .activeDataSet, subset, ", Hess=TRUE)", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="polr", model=TRUE)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    modelFormula()
    subsetBox(model=TRUE)
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(outerOperatorsFrame, sticky="w")
    tkgrid(formulaFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=lhsEntry)
    }
    
multinomialLogitModel <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkVariables(2)) return()
    initializeDialog(title="Multinomial Logit Model")
    currentModel <- if (!is.null(.activeModel)) 
        eval(parse(text=paste("class(", .activeModel, ")[1] == 'multinom'", sep="")), 
            envir=.GlobalEnv) 
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(eval(parse(text=.activeModel), 
            envir=.GlobalEnv))
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    assign(".modelNumber", .modelNumber + 1, envir=.GlobalEnv)
    modelName <- tclVar(paste("MLM.", .modelNumber, sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    onOK <- function(){
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            errorCondition(recall=multinomialLogitModel, message=paste('"', modelValue, '" is not a valid name.', sep=""), model=TRUE)
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == "<all valid cases>" || trim.blanks(subset) == ""){
            subset <- ""
            assign(".modelWithSubset", FALSE, envir=.GlobalEnv)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            assign(".modelWithSubset", TRUE, envir=.GlobalEnv)            
            }
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=multinomialLogitModel, message="Left-hand side of model empty.", model=TRUE) 
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=multinomialLogitModel, message="Right-hand side of model empty.", model=TRUE)
            return()
            }
        if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=eval(parse(text=.activeDataSet), envir=.GlobalEnv)))){
            errorCondition(recall=multinomialLogitModel, message="Response variable must be a factor")
            return()
            }
        if (is.element(modelValue, listMultinomialLogitModels())) {
            if ("no" == tclvalue(checkReplace(modelValue, type="Model"))){
                assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv) 
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                linearModel()
                return()
                }
            }
        activeModel(modelValue)
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        command <- paste("multinom(", formula,
            ", data=", .activeDataSet, subset, ", trace=FALSE)", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ", cor=FALSE)", sep=""))
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="multinom", model=TRUE)
    tkgrid(tklabel(modelFrame, text="Enter name for model:"), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    modelFormula()
    subsetBox(model=TRUE)
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(outerOperatorsFrame, sticky="w")
    tkgrid(formulaFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=lhsEntry)
    }

formulaFields <- function(model, glm=FALSE){
    formula <- as.character(model$call$formula)
    lhs <- formula[2]
    rhs <- formula[3]
    data <- as.character(model$call$data)
    which.subset <- which("subset" == names(model$call))
    subset <- if (0 == length(which.subset)) ""
        else as.character(model$call)[[which.subset]]
    if (glm) {
        fam <- as.character(model$call$family)
        family <- fam[1]
        link <- fam[2]
        }
    else {
        family <- NULL
        link <- NULL
        }
    list(lhs=lhs, rhs=rhs, data=data, subset=subset, family=family, link=link)
    }
