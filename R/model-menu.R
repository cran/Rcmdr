# Model menu dialogs

# last modified 27 April 04 by J. Fox

selectActiveModel <- function(){
    models <- union(listLinearModels(), listGeneralizedLinearModels())
    if (length(models) == 0){
        tkmessageBox(message="There are no models from which to choose.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Select Model")
    modelsFrame <- tkframe(top)
    modelsBox <- tklistbox(modelsFrame, height=min(4, length(models)),
        selectmode="single", background="white")
    modelsScroll <- tkscrollbar(modelsFrame, repeatinterval=5, 
        command=function(...) tkyview(modelsBox, ...))
    tkconfigure(modelsBox, yscrollcommand=function(...) tkset(modelsScroll, ...))
    for (mod in models) tkinsert(modelsBox, "end", mod)
    tkselection.set(modelsBox, if (is.null(.activeModel)) 0 else which(.activeModel == models) - 1)
    buttonsFrame <- tkframe(top)
    onOK <- function(){
    model <- models[as.numeric(tkcurselection(modelsBox)) + 1]
    dataSet <- eval(parse(text=paste("as.character(", model, "$call$data)")))
    if (length(dataSet) == 0){
        tkmessageBox(message="There is no dataset associated with this model.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            return()
            }
        dataSets <- listDataSets()
        if (!is.element(dataSet, dataSets)){
        tkmessageBox(message=paste("The dataset associated with this model, ", 
                dataSet, ", is not in memory.", sep=""),
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            tkfocus(.commander)
            return()
            }
        if (dataSet != .activeDataSet) activeDataSet(dataSet)
        activeModel(model)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    nameFrame <- tkframe(top)
    tkgrid(tklabel(nameFrame, fg="blue", text="Current Model: "), 
        tklabel(nameFrame, text=tclvalue(.modelName)), sticky="w")
    tkgrid(nameFrame, sticky="w", columnspan="2")
    tkgrid(tklabel(top, text="Models (pick one)"), sticky="w")
    tkgrid(modelsBox, modelsScroll, sticky="nw")
    tkgrid(modelsFrame, columnspan="2", sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(modelsScroll, sticky="ns")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

summarizeModel <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint(paste("summary(", .activeModel, ")", sep=""))
    }

plotModel <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint("par(mfrow=c(2,2))")
    doItAndPrint(paste("plot(", .activeModel, ")", sep=""))
    doItAndPrint("par(mfrow=c(1,1))")
    }

CRPlots <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint(paste("cr.plots(", .activeModel, ", ask=FALSE)", sep=""))
    }

AVPlots <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    response <- tclvalue(tkmessageBox(message="Identify points with mouse?", 
        icon="question", type="yesno", default="no"))
    doItAndPrint(paste("av.plots(", .activeModel, ", ask=FALSE, identify.points=",
        response=="yes", ")", sep=""))
    }

anovaTable <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint(paste("Anova(", .activeModel, ")", sep=""))
    }

VIF <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (class(get(.activeModel, envir=.GlobalEnv))[1] != "lm"){
        tkmessageBox(message="Variance-inflation factors available\nonly for linear models.", 
            icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint(paste("vif(", .activeModel, ")", sep=""))
    }
            
influencePlot <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    response <- tclvalue(tkmessageBox(message="Identify points with mouse?", 
        icon="question", type="yesno", default="no"))
    labels <- if (response == "no") ", labels=FALSE" else ""
    doItAndPrint(paste("influence.plot(", .activeModel, labels, ")", sep=""))
    }  
    
effectPlots <- function(){
          if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    doItAndPrint('trellis.device(bg="white")')
    command <- paste("plot(all.effects(", .activeModel, "), ask=FALSE)", sep="")
    justDoIt(command)
    logger(command)
    }

addObservationStatistics <- function(){
    checkReplace <- function(name){
        tkmessageBox(message=paste("Variable", name, "already exists.\nOverwrite variable?"),
            icon="warning", type="yesno", default="no")
        }
    addVariable <- function(name, values){
        variable <- paste(.activeModel, ".", name, sep="")
        if (is.element(variable, .variables)) {
            ans <- checkReplace(variable)
            if (tclvalue(ans) == "no") return()
            }
        command <- paste(name, "(", .activeModel, ")", sep="")
#        justDoIt(paste(.activeDataSet, "$", variable, " <<- ", command, sep=""))
        justDoIt(paste(.activeDataSet, "$", variable, " <- ", command, sep=""))
        logger(paste(.activeDataSet, "$", variable, " <- ", command, sep=""))
        }
    if (is.null(.activeModel)){
            tkmessageBox(message="There is no active model.", icon="error", type="ok")
            tkfocus(.commander)
            return()
            }
    if (.modelWithSubset){
        tkmessageBox(message=
            paste("Observation statistics not available\nfor a model fit to a subset of the data."),
            icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Add Observation Statistics to Data")
    selectFrame <- tkframe(top)
    fittedVariable <- tclVar("1")
    residualsVariable <- tclVar("1")
    rstudentVariable <- tclVar("1")
    hatvaluesVariable <- tclVar("1")
    cookdVariable <- tclVar("1")
    obsNumberExists <- is.element("obsNumber", .variables)
    obsNumberVariable <- tclVar(if(obsNumberExists) "0" else "1")
    fittedCheckBox <- tkcheckbutton(selectFrame, variable=fittedVariable)
    residualsCheckBox <- tkcheckbutton(selectFrame, variable=residualsVariable)
    rstudentCheckBox <- tkcheckbutton(selectFrame, variable=rstudentVariable)
    hatvaluesCheckBox <- tkcheckbutton(selectFrame, variable=hatvaluesVariable)
    cookdCheckBox <- tkcheckbutton(selectFrame, variable=cookdVariable)
    obsNumberCheckBox <- tkcheckbutton(selectFrame, variable=obsNumberVariable)
    onOK <- function(){
        if (tclvalue(fittedVariable) == 1) addVariable("fitted")
        if (tclvalue(residualsVariable) == 1) addVariable("residuals")
        if (tclvalue(rstudentVariable) == 1) addVariable("rstudent")
        if (tclvalue(hatvaluesVariable) == 1) addVariable("hatvalues")
        if (tclvalue(cookdVariable) == 1) addVariable("cookd")
        if (tclvalue(obsNumberVariable) == 1){
            proceed <- if (obsNumberExists) tclvalue(checkReplace("obsNumber")) else "yes"
            if (proceed == "yes") {
                command <- paste(.activeDataSet, "$obsNumber <- 1:nrow(", .activeDataSet, ")", sep="")
                justDoIt(command)
                logger(command)
                }
            }
        activeDataSet(.activeDataSet)
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
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(influence.measures)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(selectFrame, text="Fitted values"), fittedCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Residuals"), residualsCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Studentized residuals"), rstudentCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Hat-values"), hatvaluesCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Cook's distances"), cookdCheckBox, sticky="w")  
    tkgrid(tklabel(selectFrame, text="Observation indices"), obsNumberCheckBox, sticky="w")
    tkgrid(selectFrame, sticky="w")  
        tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
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

residualQQPlot <- function(){
    if (is.null(.activeModel)){
            tkmessageBox(message="There is no active model.", icon="error", type="ok")
            tkfocus(.commander)
            return()
            }
    top <- tktoplevel()
    tkwm.title(top, "Residual Quantile-Comparison Plot")
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
        help(qq.plot.lm)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(selectFrame, text="Simulated confidence envelope"), simulateCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Identify points with mouse"), identifyCheckBox, sticky="w")
    tkgrid(selectFrame, sticky="w")  
        tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
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

testLinearHypothesis <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    env <- environment()
    top <- tktoplevel()
    tkwm.title(top, "Test Linear Hypothesis")
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
            tkmessageBox(message=paste("Number of valid entries in rhs vector (", length(rhs), ")\n",
                "not equal to number of rows (", nrows,")", 
                sep=""), icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            testLinearHypothesis()
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
        help(linear.hypothesis)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(rowsFrame, text="Number of Rows:"), rowsSlider, rowsShow, sticky="w")
    tkgrid(rowsFrame, sticky="w")
    tkgrid(tklabel(top, text="Enter hypothesis matrix and right-hand side vector:", fg="blue"), sticky="w")
    tkgrid(outerTableFrame, sticky="w")
    tkgrid(tklabel(top, text=""))
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
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

compareModels <- function(){
    models <- union(listLinearModels(), listGeneralizedLinearModels())
    if (length(models) < 2){
        tkmessageBox(message="There are fewer than two models.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Compare Models")
    modelsFrame1 <- tkframe(top)
    modelsBox1 <- tklistbox(modelsFrame1, height=min(4, length(models)),
        selectmode="single", background="white", exportselection="FALSE")
    modelsScroll1 <- tkscrollbar(modelsFrame1, repeatinterval=5, 
        command=function(...) tkyview(modelsBox1, ...))
    tkconfigure(modelsBox1, yscrollcommand=function(...) tkset(modelsScroll1, ...))
    for (mod in models) tkinsert(modelsBox1, "end", mod)
    modelsFrame2 <- tkframe(top)
    modelsBox2 <- tklistbox(modelsFrame2, height=min(4, length(models)),
        selectmode="single", background="white", exportselection="FALSE")
    modelsScroll2 <- tkscrollbar(modelsFrame2, repeatinterval=5, 
        command=function(...) tkyview(modelsBox2, ...))
    tkconfigure(modelsBox2, yscrollcommand=function(...) tkset(modelsScroll2, ...))
    for (mod in models) tkinsert(modelsBox2, "end", mod)
    buttonsFrame <- tkframe(top)
    onOK <- function(){
        model1 <- models[as.numeric(tkcurselection(modelsBox1)) + 1]
        model2 <- models[as.numeric(tkcurselection(modelsBox2)) + 1]
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
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(anova)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    tkgrid(tklabel(modelsFrame1, text="First model (pick one)"), sticky="w")
    tkgrid(modelsBox1, modelsScroll1, sticky="nw")
    tkgrid(tklabel(modelsFrame2, text="Second model (pick one)"), sticky="w")
    tkgrid(modelsBox2, modelsScroll2, sticky="nw")
    tkgrid(modelsFrame1, modelsFrame2, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(modelsScroll1, sticky="ns")
    tkgrid.configure(modelsScroll2, sticky="ns")
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

BreuschPaganTest <- function(){
    checkAddOperator <- function(rhs){
        rhs.chars <- rev(strsplit(rhs, "")[[1]])
        if (length(rhs.chars) < 1) return(FALSE)
        check.char <- if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                rhs.chars[1] else rhs.chars[2]
        !is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%"))
        }
    if (activeModel() == FALSE) {
        tkmessageBox(message="There is no active model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (eval(parse(text=paste("class(", .activeModel, ")[1] != 'lm'", sep="")))){
        tkmessageBox(message="Breusch-Pagan test requires a linear model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Breusch-Pagan Test")
    tkgrid(tklabel(top, text="Score Test for Nonconstant Error Variance"), sticky="w")
    variables <- paste(.variables, ifelse(is.element(.variables, .factors), "[factor]", ""))
    optionsFrame <- tkframe(top)
    xFrame <- tkframe(optionsFrame)
    xBox <- tklistbox(xFrame, height=min(4, length(.variables)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in variables) tkinsert(xBox, "end", x)
    onDoubleClick <- function(){
        var <- as.character(tkget(xBox, "active"))[1]
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
        var <- tclvalue(varVariable)
        type <- if (var == "fitted") paste(", varformula = ~ fitted.values(",
                    .activeModel, ")", sep="") 
                else if (var == "predictors") ""
                else paste(", varformula = ~", tclvalue(rhsVariable), 
                ", data=", .activeDataSet, sep="")
        student <- if (tclvalue(studentVariable) == 1) "TRUE" else "FALSE"
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("bptest(", .activeModel, type, ", studentize=", student,
             ")", sep="")
        doItAndPrint(command)  
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    operatorsFrame <- tkframe(optionsFrame)
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
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(bptest)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    varVariable <- tclVar("fitted")
    fittedButton <- tkradiobutton(optionsFrame, variable=varVariable, value="fitted")
    predictorsButton <- tkradiobutton(optionsFrame, variable=varVariable, value="predictors")
    otherButton <- tkradiobutton(optionsFrame, variable=varVariable, value="other")
    studentVariable <- tclVar("0")
    studentCheckBox <- tkcheckbutton(optionsFrame, variable=studentVariable)
    rhsVariable <- tclVar("")
    rhsEntry <- tkentry(optionsFrame, width="50", textvariable=rhsVariable)
    rhsXscroll <- tkscrollbar(optionsFrame, repeatinterval=10,
        orient="horizontal", command=function(...) tkxview(rhs, ...))
    tkconfigure(rhsEntry, xscrollcommand=function(...) tkset(rhsXscroll, ...))  
    tkgrid(tklabel(optionsFrame, text="Studentized\ntest statistic", justify="left"),
        studentCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Variance Formula", fg="blue"), sticky="w") 
    tkgrid(tklabel(optionsFrame, text="Fitted values"), fittedButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Explanatory variables"), predictorsButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Other (specify)"), otherButton, 
        tklabel(optionsFrame, text="   ~ "), rhsEntry, sticky="w")
    tkgrid(tklabel(optionsFrame, text=""), tklabel(optionsFrame, text=""),
        tklabel(optionsFrame, text=""), rhsXscroll, sticky="w")
    tkgrid.configure(rhsXscroll, sticky="ew")
    tkgrid(plusButton, timesButton, colonButton, slashButton, inButton, minusButton,
        powerButton, leftParenButton, rightParenButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text=""),tklabel(optionsFrame, text=""),
        tklabel(optionsFrame, text=""), operatorsFrame, sticky="w")
    tkgrid(tklabel(optionsFrame, text=""),tklabel(optionsFrame, text=""),
        tklabel(optionsFrame, text=""),
        tklabel(optionsFrame, text="Variables (double-click to formula)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(tklabel(optionsFrame, text=""),tklabel(optionsFrame, text=""),
        tklabel(optionsFrame, text=""), xFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkbind(top, "<Return>", onOK)
    tkbind(xBox, "<Double-ButtonPress-1>", onDoubleClick)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

DurbinWatsonTest <- function(){
    if (activeModel() == FALSE) {
        tkmessageBox(message="There is no active model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (eval(parse(text=paste("class(", .activeModel, ")[1] != 'lm'", sep="")))){
        tkmessageBox(message="Durbin-Watson test requires a linear model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Durbin-Waton Test")
    tkgrid(tklabel(top, text="Test for First-Order Error Autocorrelation"), sticky="w")
    onOK <- function(){
        altHypothesis <- tclvalue(altHypothesisVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("dwtest(", .activeModel, ', alternative="', altHypothesis,
             '")', sep="")
        doItAndPrint(command)  
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(dwtest)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    optionsFrame <- tkframe(top)
    altHypothesisVariable <- tclVar("greater")
    greaterButton <- tkradiobutton(optionsFrame, variable=altHypothesisVariable, value="greater")
    notequalButton <- tkradiobutton(optionsFrame, variable=altHypothesisVariable, value="two.sided")
    lessButton <- tkradiobutton(optionsFrame, variable=altHypothesisVariable, value="less")
    tkgrid(tklabel(top, text="Alternative Hypothesis", fg="blue"), sticky="w") 
    tkgrid(tklabel(optionsFrame, text="rho >  0"), greaterButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="rho != 0"), notequalButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="rho <  0"), lessButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
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

RESETtest <- function(){
    if (activeModel() == FALSE) {
        tkmessageBox(message="There is no active model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (eval(parse(text=paste("class(", .activeModel, ")[1] != 'lm'", sep="")))){
        tkmessageBox(message="RESET test requires a linear model.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "RESET Test")
    tkgrid(tklabel(top, text="Test for Nonlinearity"), sticky="w")
    onOK <- function(){
        type <- tclvalue(typeVariable)
        square <- tclvalue(squareVariable)
        cube <- tclvalue(cubeVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        if (square == "0" && cube == "0"){
            tkmessageBox(message="No powers are checked.", 
                icon="error", type="ok")
            tkfocus(.commander)
            RESETtest()
            return()
            }
        powers <- if (square == "1" && cube == "1") "2:3"
            else if (square == "1" && cube == "0") "2"
            else if (square == "0" && cube == "1") "3"
        command <- paste("reset(", .activeModel, ", power=", powers,
            ', type="', type, '")', sep="")
        doItAndPrint(command)  
        tkfocus(.commander)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(reset)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    optionsFrame <- tkframe(top)
    squareVariable <- tclVar("1")
    squareCheckBox <- tkcheckbutton(optionsFrame, variable=squareVariable)
    cubeVariable <- tclVar("1")
    cubeCheckBox <- tkcheckbutton(optionsFrame, variable=cubeVariable)
    typeVariable <- tclVar("regressor")
    regressorButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="regressor")
    fittedButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="fitted")
    princompButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="princomp")
    tkgrid(tklabel(optionsFrame, text="Powers to Include", fg="blue"), sticky="w")
    tkgrid(tklabel(optionsFrame, text="2 (squares)"), squareCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="3 (cubes)   "), cubeCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Type of Test", fg="blue"), sticky="w") 
    tkgrid(tklabel(optionsFrame, text="Explanatory variables"), regressorButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Fitted values"), fittedButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="First principal component"), princompButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
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

outlierTest <- function(){
    doItAndPrint(paste("outlier.test(", .activeModel, ")", sep=""))
    }
