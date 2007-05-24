ordinalRegressionModel <- function(){
    require("MASS")
    initializeDialog(title=gettextRcmdr("Ordinal Regression Model"))
    .activeModel <- ActiveModel()
    .activeDataSet <- ActiveDataSet()
    currentModel <- if (!is.null(.activeModel)) 
        eval(parse(text=paste("class(", .activeModel, ")[1] == 'polr'", sep="")), 
            envir=.GlobalEnv) 
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(eval(parse(text=.activeModel), 
            envir=.GlobalEnv))
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    UpdateModelNumber()
    modelName <- tclVar(paste("OrdRegModel.", getRcmdr("modelNumber"), sep=""))
    modelFrame <- tkframe(top)
    model <- tkentry(modelFrame, width="20", textvariable=modelName)
    radioButtons(name="modelType", 
        buttons=c("logistic", "probit", "cloglog", "cauchit"), 
        labels=gettextRcmdr(c("Proportional-odds logit", "Ordered probit", "Ordered complementary log-log", "Ordered cauchit")),
        title=gettextRcmdr("Type of Model"))
    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()
        if (!is.valid.name(modelValue)){
            errorCondition(recall=proportionalOddsModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
            subset <- ""
            putRcmdr("modelWithSubset", FALSE)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            putRcmdr("modelWithSubset", TRUE)
            }
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=proportionalOddsModel, message=gettextRcmdr("Left-hand side of model empty."), model=TRUE) 
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=proportionalOddsModel, message=gettextRcmdr("Right-hand side of model empty."), model=TRUE)
            return()
            }
        if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=eval(parse(text=.activeDataSet), envir=.GlobalEnv)))){
            errorCondition(recall=proportionalOddsModel, message=gettextRcmdr("Response variable must be a factor"))
            return()
            }
        if (is.element(modelValue, listProportionalOddsModels())) {
            if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
                UpdateModelNumber(-1)
                proportionalOddsModel()
                return()
                }
            }
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        command <- paste("polr(", formula, ', method="', tclvalue(modelTypeVariable),
            '", data=', .activeDataSet, subset, ", Hess=TRUE)", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        activeModel(modelValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="polr", model=TRUE)
    tkgrid(tklabel(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    modelFormula()
    subsetBox(model=TRUE)
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(outerOperatorsFrame, sticky="w")
    tkgrid(formulaFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(modelTypeFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=7, columns=1, focus=lhsEntry, preventDoubleClick=TRUE)
    }
