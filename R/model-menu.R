# Model menu dialogs

# last modified 23 May 03 by J. Fox

selectActiveModel <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Select Model")
    models <- union(listLinearModels(), listGeneralizedLinearModels())
    modelsFrame <- tkframe(top)
    modelsScroll <- tkscrollbar(modelsFrame, repeatinterval=5, 
        command=function(...) tkyview(dataSetsBox, ...))
    modelsBox <- tklistbox(modelsFrame, height=min(4, length(models)),
        selectmode="single", background="white",
        yscrollcommand=function(...) tkset(modelsScroll, ...))
    for (mod in models) tkinsert(modelsBox, "end", mod)
    tkselection.set(modelsBox, if (is.null(.activeModel)) 0 else which(.activeModel == models) - 1)
    onOK <- function(){
        activeModel(models[as.numeric(tkcurselection(modelsBox)) + 1])
        tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    OKbutton <- tkbutton(top, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    cancelButton <- tkbutton(top, text="Cancel", width="12", command=onCancel)
    tkgrid(tklabel(top, fg="red", text=paste("Current model:", tclvalue(.modelName))))
    tkgrid(tklabel(top, text="Models (pick one)"), sticky="w")
    tkgrid(modelsBox, modelsScroll, sticky="nw")
    tkgrid.configure(modelsScroll, sticky="ns")
    tkgrid(modelsFrame, columnspan="2", sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

summarizeModel <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        return()
        }
    doItAndPrint(paste("summary(", .activeModel, ")", sep=""))
    }

plotModel <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        return()
        }
    doItAndPrint("par(mfrow=c(2,2))")
    doItAndPrint(paste("plot(", .activeModel, ")", sep=""))
    doItAndPrint("par(mfrow=c(1,1))")
    }

CRPlots <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        return()
        }
    doItAndPrint(paste("cr.plots(", .activeModel, ", ask=FALSE)", sep=""))
    }

AVPlots <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
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
        return()
        }
    doItAndPrint(paste("Anova(", .activeModel, ")", sep=""))
    }

VIF <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        return()
        }
    if (class(eval(parse(text=.activeModel)))[1] != "lm"){
        tkmessageBox(message="Variance-inflation factors available\nonly for linear models.", 
            icon="error", type="ok")
        return()
        }
    doItAndPrint(paste("vif(", .activeModel, ")", sep=""))
    }
            
influencePlot <- function(){
    if (is.null(.activeModel)) {
        tkmessageBox(message="There is no active model.", icon="error", type="ok")
        return()
        }
    response <- tclvalue(tkmessageBox(message="Identify points with mouse?", 
        icon="question", type="yesno", default="no"))
    labels <- if (response == "no") ", labels=FALSE" else ""
    doItAndPrint(paste("influence.plot(", .activeModel, labels, ")", sep=""))
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
        justDoIt(paste(.activeDataSet, "$", variable, " <<- ", command, sep=""))
        logger(paste(.activeDataSet, "$", variable, " <- ", command, sep=""))
        }
    if (is.null(.activeModel)){
            tkmessageBox(message="There is no active model.", icon="error", type="ok")
            return()
            }
    if (.modelWithSubset){
        tkmessageBox(message=
            paste("Observation statistics not available\nfor a model fit to a subset of the data."),
            icon="error", type="ok")
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
    fittedCheckBox <- tkcheckbutton(selectFrame, variable=fittedVariable)
    residualsCheckBox <- tkcheckbutton(selectFrame, variable=residualsVariable)
    rstudentCheckBox <- tkcheckbutton(selectFrame, variable=rstudentVariable)
    hatvaluesCheckBox <- tkcheckbutton(selectFrame, variable=hatvaluesVariable)
    cookdCheckBox <- tkcheckbutton(selectFrame, variable=cookdVariable)
    onOK <- function(){
        if (tclvalue(fittedVariable) == 1) addVariable("fitted")
        if (tclvalue(residualsVariable) == 1) addVariable("residuals")
        if (tclvalue(rstudentVariable) == 1) addVariable("rstudent")
        if (tclvalue(hatvaluesVariable) == 1) addVariable("hatvalues")
        if (tclvalue(cookdVariable) == 1) addVariable("cookd")
        activeDataSet(.activeDataSet)
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
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(influence.measures)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(selectFrame, text="Fitted values"), fittedCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Residuals"), residualsCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Studentized residuals"), rstudentCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Hat-values"), hatvaluesCheckBox, sticky="w")
    tkgrid(tklabel(selectFrame, text="Cook's distances"), cookdCheckBox, sticky="w")  
    tkgrid(selectFrame, sticky="w")  
        tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }
