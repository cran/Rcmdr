# Model menu dialogs

# last modified 19 Nov 04 by J. Fox

selectActiveModel <- function(){
    models <- listAllModels()
    if (length(models) == 0){
        tkmessageBox(message="There are no models from which to choose.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    initializeDialog(title="Select Model")
    initial <- if (is.null(.activeModel)) NULL else which(.activeModel == models) - 1
    modelsBox <- variableListBox(top, models, title="Models (pick one)", 
        initialSelection=initial)
    onOK <- function(){
        model <- getSelection(modelsBox)
        if (length(model) == 0) {
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            return()
            }
        dataSet <- eval(parse(text=paste("as.character(", model, "$call$data)")))
        if (length(dataSet) == 0){
            errorCondition(message="There is no dataset associated with this model.")
            return()
            }
        dataSets <- listDataSets()
        if (!is.element(dataSet, dataSets)){
            errorCondition(message=paste("The dataset associated with this model, ", 
                dataSet, ", is not in memory.", sep=""))
            return()
            }
        if (is.null(.activeDataSet) || (dataSet != .activeDataSet)) activeDataSet(dataSet)
        activeModel(model)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    OKCancelHelp()
    nameFrame <- tkframe(top)
    tkgrid(tklabel(nameFrame, fg="blue", text="Current Model: "), 
        tklabel(nameFrame, text=tclvalue(.modelName)), sticky="w")
    tkgrid(nameFrame, sticky="w", columnspan="2")
    tkgrid(getFrame(modelsBox), columnspan="2", sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=3, columns=2)
    }

summarizeModel <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("summary", .activeModel)) return()
    doItAndPrint(paste("summary(", .activeModel, ")", sep=""))
    }

plotModel <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("plot", .activeModel)) return()
    doItAndPrint("par(mfrow=c(2,2))")
    doItAndPrint(paste("plot(", .activeModel, ")", sep=""))
    doItAndPrint("par(mfrow=c(1,1))")
    }

CRPlots <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("cr.plot", .activeModel)) return()
    doItAndPrint(paste("cr.plots(", .activeModel, ", ask=FALSE)", sep=""))
    }

AVPlots <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("av.plot", .activeModel)) return()
    response <- tclvalue(tkmessageBox(message="Identify points with mouse?", 
        icon="question", type="yesno", default="no"))
    doItAndPrint(paste("av.plots(", .activeModel, ", ask=FALSE, identify.points=",
        response=="yes", ")", sep=""))
    }

anovaTable <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("Anova", .activeModel)) return()
    doItAndPrint(paste("Anova(", .activeModel, ")", sep=""))
    }

VIF <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("vif", .activeModel)) return()
    doItAndPrint(paste("vif(", .activeModel, ")", sep=""))
    }
            
influencePlot <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("influence.plot", .activeModel)) return()
    response <- tclvalue(tkmessageBox(message="Identify points with mouse?", 
        icon="question", type="yesno", default="no"))
    labels <- if (response == "no") ", labels=FALSE" else ""
    doItAndPrint(paste("influence.plot(", .activeModel, labels, ")", sep=""))
    }  
    
effectPlots <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("effect", .activeModel)) return()
    doItAndPrint('trellis.device(theme="col.whitebg")')
    command <- paste("plot(all.effects(", .activeModel, "), ask=FALSE)", sep="")
    justDoIt(command)
    logger(command)
    NULL
    }

addObservationStatistics <- function(){
    addVariable <- function(name, values){
        variable <- paste(.activeModel, ".", name, sep="")
        if (is.element(variable, .variables)) {
            ans <- checkReplace(variable)
            if (tclvalue(ans) == "no") return()
            }
        command <- paste(name, "(", .activeModel, ")", sep="")
        justDoIt(paste(.activeDataSet, "$", variable, " <- ", command, sep=""))
        logger(paste(.activeDataSet, "$", variable, " <- ", command, sep=""))
        }
    if (!checkActiveModel()) return()
    if (.modelWithSubset){
        tkmessageBox(message=
            paste("Observation statistics not available\nfor a model fit to a subset of the data."),
            icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    initializeDialog(title="Add Observation Statistics to Data")
    obsNumberExists <- is.element("obsNumber", .variables)
    checkBoxes(frame="selectFrame", boxes=c("fitted", "residuals", "rstudent", "hatvalues", "cookd", "obsNumbers"),
        initialValues=c("1", "1", "1", "1", "1", if(obsNumberExists) "0" else "1"),
        labels=c("Fitted values", "Residuals", "Studentized residuals", "Hat-values", "Cook's distances", 
        "Observation indices"))
    onOK <- function(){
        if (tclvalue(fittedVariable) == 1) {
            if (checkMethod("fitted", .activeModel, default=TRUE)) addVariable("fitted")
            }
        if (tclvalue(residualsVariable) == 1) {
            if (checkMethod("residuals", .activeModel, default=TRUE)) addVariable("residuals")
            }
        if (tclvalue(rstudentVariable) == 1) {
            if (checkMethod("rstudent", .activeModel)) addVariable("rstudent")
            }
        if (tclvalue(hatvaluesVariable) == 1) {
            if (checkMethod("hatvalues", .activeModel)) addVariable("hatvalues")
            }
        if (tclvalue(cookdVariable) == 1) {
            if (checkMethod("cooks.distance", .activeModel)) addVariable("cookd")
            }
        if (tclvalue(obsNumbersVariable) == 1){
            proceed <- if (obsNumberExists) tclvalue(checkReplace("obsNumber")) else "yes"
            if (proceed == "yes") {
                command <- paste(.activeDataSet, "$obsNumber <- 1:nrow(", .activeDataSet, ")", sep="")
                justDoIt(command)
                logger(command)
                }
            }
        activeDataSet(.activeDataSet, flushModel=FALSE)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="influence.measures")
    tkgrid(selectFrame, sticky="w")  
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=5, columns=1)
    }

residualQQPlot <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("qq.plot", .activeModel)) return()
    initializeDialog(title="Residual Quantile-Comparison Plot")
    selectFrame <- tkframe(top)
    simulateVar <- tclVar("1")
    identifyVar <- tclVar("0")
    simulateCheckBox <- tkcheckbutton(selectFrame, variable=simulateVar)
    identifyCheckBox <- tkcheckbutton(selectFrame, variable=identifyVar)
    onOK <- function(){
        tkdestroy(top)
        simulate <- tclvalue(simulateVar) == 1
        identify <- if (tclvalue(identifyVar) == 1) paste("names(residuals(", .activeModel, "))",
            sep="") else "FALSE"
        command <- paste("qq.plot(", .activeModel, ", simulate=", simulate, ", labels=", identify,
            ")", sep="")
        doItAndPrint(command)
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="qq.plot.lm")
    tkgrid(tklabel(selectFrame, text="Simulated confidence envelope"), simulateCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Identify points with mouse"), identifyCheckBox, sticky="w")
    tkgrid(selectFrame, sticky="w")  
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }

testLinearHypothesis <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("linear.hypothesis", .activeModel)) return()
    env <- environment()
    initializeDialog(title="Test Linear Hypothesis")
    outerTableFrame <- tkframe(top)
    assign(".tableFrame", tkframe(outerTableFrame), envir=env)
    setUpTable <- function(...){
        tkdestroy(get(".tableFrame", envir=env))
        assign(".tableFrame", tkframe(outerTableFrame), envir=env)
        nrows <- as.numeric(tclvalue(rowsValue))
        col.names <- eval(parse(text=paste("names(coef(", .activeModel, "))")))
        col.names <- substring(paste(abbreviate(col.names, 12), "            "), 1, 12)
        make.col.names <- "tklabel(.tableFrame, text='')"
        for (j in 1:ncols) {
            make.col.names <- paste(make.col.names, ", ", 
                "tklabel(.tableFrame, text='", col.names[j], "')", sep="")
            }
        make.col.names <- paste(make.col.names, ", tklabel(.tableFrame, text='          ')",
            ", tklabel(.tableFrame, text='Right-hand side')", sep="")
        eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
        for (i in 1:nrows){   
            varname <- paste(".tab.", i, ".1", sep="") 
            rhs.name <- paste(".rhs.", i, sep="")
            assign(varname, tclVar("0") , envir=env)
            assign(rhs.name, tclVar("0"), envir=env)
            make.row <- paste("tklabel(.tableFrame, text=", i, ")")
            make.row <- paste(make.row, ", ", "tkentry(.tableFrame, width='5', textvariable=", 
                varname, ")", sep="")
            for (j in 2:ncols){
                varname <- paste(".tab.", i, ".", j, sep="")
                assign(varname, tclVar("0"), envir=env)
                make.row <- paste(make.row, ", ", "tkentry(.tableFrame, width='5', textvariable=", 
                    varname, ")", sep="")
                }
            make.row <- paste(make.row, ", tklabel(.tableFrame, text='     '),",
                "tkentry(.tableFrame, width='5', textvariable=", rhs.name, ")", sep="")
            eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
            }
        tkgrid(get(".tableFrame", envir=env), sticky="w")
        }
    ncols <- eval(parse(text=paste("length(coef(", .activeModel, "))")))
    rowsFrame <- tkframe(top)
    rowsValue <- tclVar("1")
    rowsSlider <- tkscale(rowsFrame, from=1, to=ncols, showvalue=FALSE, variable=rowsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    rowsShow <- tklabel(rowsFrame, textvariable=rowsValue, width=2, justify="right")
    onOK <- function(){
        nrows <- as.numeric(tclvalue(rowsValue))
        cell <- 0
        values <- rep(NA, nrows*ncols)
        rhs <- rep(NA, nrows)
        for (i in 1:nrows){
            rhs.name <- paste(".rhs.", i, sep="")
            rhs[i] <- as.numeric(eval(parse(text=paste("tclvalue(", rhs.name,")", sep=""))))
            for (j in 1:ncols){
                cell <- cell+1
                varname <- paste(".tab.", i, ".", j, sep="")
                values[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
                }
            }
        values <- na.omit(values)
        if (length(values) != nrows*ncols){
            tkmessageBox(message=paste("Number of valid entries in hypothesis matrix(", length(values), ")\n",
                "not equal to number of rows (", nrows,") * number of columns (", ncols,").", 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            testLinearHypothesis()
            return()
            }
        if (qr(matrix(values, nrows, ncols, byrow=TRUE))$rank < nrows) {
            tkmessageBox(message="Hypothesis matrix is not of full row rank", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            testLinearHypothesis()
            return()
            }            
        rhs <- na.omit(rhs)
        if (length(rhs) != nrows){
            errorCondition(recall=testLinearHypothesis, message=paste("Number of valid entries in rhs vector (", 
                length(rhs), ")\n", "not equal to number of rows (", nrows,")", sep=""))
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("matrix(c(", paste(values, collapse=","), "), ", nrows, ", ", ncols,
            ", byrow=TRUE)", sep="")
        assign(".Hypothesis", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".Hypothesis <- ", command, sep=""))
        command <- paste("c(", paste(rhs, collapse=","), ")", sep="")
        assign(".RHS", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".RHS <- ", command, sep=""))
        command <- paste("linear.hypothesis(", .activeModel, ", .Hypothesis, rhs=.RHS)", sep="")
        doItAndPrint(command)
        justDoIt("remove(.Hypothesis, .RHS, envir=.GlobalEnv)") 
        logger("remove(.Hypothesis, .RHS)")                                              
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="linear.hypothesis")
    tkgrid(tklabel(rowsFrame, text="Number of Rows:"), rowsSlider, rowsShow, sticky="w")
    tkgrid(rowsFrame, sticky="w")
    tkgrid(tklabel(top, text="Enter hypothesis matrix and right-hand side vector:", fg="blue"), sticky="w")
    tkgrid(outerTableFrame, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)       
    } 

compareModels <- function(){
    models <- listAllModels()
    if (length(models) < 2){
        tkmessageBox(message="There are fewer than two models.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    initializeDialog(title="Compare Models")
    modelsBox1 <- variableListBox(top, models, title="First model (pick one)")
    modelsBox2 <- variableListBox(top, models, title="Second model (pick one)")
    onOK <- function(){
        model1 <- getSelection(modelsBox1)
        model2 <- getSelection(modelsBox2)
        if (length(model1) == 0 || length(model2) == 0) {
            errorCondition(recall=compareModels, message="You must select two models.")
            return()
            }
        if (!checkMethod("anova", model1)) {
            tkgrab.release(top)
            tkdestroy(top)            
            return()
            }
        if (!eval(parse(text=paste("class(", model1, ")[1] == class(", model2, ")[1]",
            sep="")), envir=.GlobalEnv)){
                tkmessageBox(message="Models are not of the same class.", 
                    icon="error", type="ok")
                tkgrab.release(top)
                tkdestroy(top)
                compareModels()
                return()
                }
        tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("anova(", model1, ",", model2, ")", sep=""))
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="anova")
    tkgrid(getFrame(modelsBox1), getFrame(modelsBox2), sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=2, columns=2)
    }
    
BreuschPaganTest <- function(){
    if(!checkActiveModel()) return()
    if (!checkClass(.activeModel, "lm")) return()
    initializeDialog(title="Breusch-Pagan Test")
    tkgrid(tklabel(top, text="Score Test for Nonconstant Error Variance", fg="blue"), sticky="w")
    optionsFrame <- tkframe(top)
    onOK <- function(){
        var <- tclvalue(varVariable)
        type <- if (var == "fitted") paste(", varformula = ~ fitted.values(",
                    .activeModel, ")", sep="") 
                else if (var == "predictors") ""
                else paste(", varformula = ~", tclvalue(rhsVariable), sep="")
        student <- if (tclvalue(studentVariable) == 1) "TRUE" else "FALSE"
        model.formula <- as.character(eval(parse(text=paste("formula(", .activeModel, ")", sep=""))))
        model.formula <- paste(model.formula[2], "~", model.formula[3])
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("bptest(", model.formula, type, ", studentize=", student,
            ", data=", .activeDataSet, ")", sep="")
        doItAndPrint(command)  
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="bptest")
    studentVariable <- tclVar("0")
    studentFrame <- tkframe(optionsFrame)
    studentCheckBox <- tkcheckbutton(studentFrame, variable=studentVariable)
    tkgrid(tklabel(studentFrame, text="Studentized test statistic", justify="left"),
        studentCheckBox, sticky="w")
    tkgrid(studentFrame, sticky="w")
    radioButtons(optionsFrame, name="var", buttons=c("fitted", "predictors", "other"), 
        labels=c("Fitted values", "Explanatory variables", "Other (specify)"), title="Variance Formula")
    tkgrid(varFrame, sticky="w")
    modelFormula(optionsFrame, hasLhs=FALSE)
    tkgrid(formulaFrame, sticky="w")
    tkgrid(outerOperatorsFrame)
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

DurbinWatsonTest <- function(){
    if (!checkActiveModel()) return()
    if (!checkClass(.activeModel, "lm")) return()
    initializeDialog(title="Durbin-Waton Test")
    tkgrid(tklabel(top, text="Test for First-Order Error Autocorrelation", fg="blue"), sticky="w")
    onOK <- function(){
        altHypothesis <- tclvalue(altHypothesisVariable)
        model.formula <- as.character(eval(parse(text=paste("formula(", .activeModel, ")", sep=""))))
        model.formula <- paste(model.formula[2], "~", model.formula[3])
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("dwtest(", model.formula, ', alternative="', altHypothesis,
             '", data=', .activeDataSet, ')', sep="")
        doItAndPrint(command)  
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="dwtest")
    radioButtons(name="altHypothesis", buttons=c("greater", "notequal", "less"), values=c("greater", "two.sided", "less"),
        labels=c("rho >  0", "rho != 0", "rho <  0"), title="Alternative Hypothesis")
    tkgrid(altHypothesisFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1)
    }

RESETtest <- function(){
    if(!checkActiveModel()) return()
    if (!checkClass(.activeModel, "lm")) return()
    initializeDialog(title="RESET Test")
    tkgrid(tklabel(top, text="Test for Nonlinearity", fg="blue"), sticky="w")
    onOK <- function(){
        type <- tclvalue(typeVariable)
        square <- tclvalue(squareVariable)
        cube <- tclvalue(cubeVariable)
        model.formula <- as.character(eval(parse(text=paste("formula(", .activeModel, ")", sep=""))))
        model.formula <- paste(model.formula[2], "~", model.formula[3])
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        if (square == "0" && cube == "0"){
            errorCondition(recall=RESETtest, message="No powers are checked.")
            return()
            }
        powers <- if (square == "1" && cube == "1") "2:3"
            else if (square == "1" && cube == "0") "2"
            else if (square == "0" && cube == "1") "3"
        command <- paste("reset(", model.formula, ", power=", powers,
            ', type="', type, '", data=', .activeDataSet, ')', sep="")
        doItAndPrint(command)  
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="reset")
    optionsFrame <- tkframe(top)
    squareVariable <- tclVar("1")
    squareCheckBox <- tkcheckbutton(optionsFrame, variable=squareVariable)
    cubeVariable <- tclVar("1")
    cubeCheckBox <- tkcheckbutton(optionsFrame, variable=cubeVariable)
    typeVariable <- tclVar("regressor")
    radioButtons(optionsFrame, name="type", buttons=c("regressor", "fitted", "princomp"),
        labels=c("Explanatory variables", "Fitted values", "First principal component"),
        title="Type of Test")
    tkgrid(tklabel(optionsFrame, text="Powers to Include", fg="blue"), sticky="w")
    tkgrid(tklabel(optionsFrame, text="2 (squares)"), squareCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="3 (cubes)   "), cubeCheckBox, sticky="w")
    tkgrid(typeFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1)
    }

outlierTest <- function(){
    if (!checkActiveModel()) return()
    if (!checkMethod("outlier.test", .activeModel)) return()
    doItAndPrint(paste("outlier.test(", .activeModel, ")", sep=""))
    }
