# Graphs menu dialogs

# last modified 20 May 03 by J. Fox

histogram <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Histogram")
    xFrame <- tkframe(top)
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        bins <- tclvalue(binsVariable)
        bins <- if (bins == "auto") "'Sturges'" else as.numeric(bins)
        scale <- tclvalue(scaleVariable) == "frequencies"
        tkgrab.release(top)
        tkdestroy(top)
        command <- paste("hist(", .activeDataSet, "$", x,
            ", breaks=", bins, sep="")
        logger(paste(command, ", freq=", scale, ")", sep=""))
        justDoIt(paste("plot(", command, "), freq=", scale, ")", sep=""))
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
        help(hist)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    scaleFrame <- tkframe(top)
    scaleVariable <- tclVar("frequencies")
    frequenciesButton <- tkradiobutton(scaleFrame, variable=scaleVariable, value="frequencies")
    densitiesButton <- tkradiobutton(scaleFrame, variable=scaleVariable, value="densities")
    binsFrame <- tkframe(top)
    binsVariable <- tclVar("auto")
    binsField <- tkentry(binsFrame, width="6", textvariable=binsVariable)
    tkgrid(tklabel(top, text="Variable"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid(xFrame, sticky="nw")    
    tkgrid(tklabel(scaleFrame, text="Axis Scaling"), columnspan=2, sticky="w")
    tkgrid(tklabel(scaleFrame, text="Frequency counts"), frequenciesButton, sticky="w")
    tkgrid(tklabel(scaleFrame, text="Densities"), densitiesButton, sticky="w")
    tkgrid(tklabel(binsFrame, text="Number of bins: "), binsField, sticky="w")
    tkgrid.configure(binsField, sticky="e")
    tkgrid(binsFrame, sticky="w")
    tkgrid(scaleFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

stemAndLeaf <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Stem and Leaf Display")
    xFrame <- tkframe(top)
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        stems <- as.character(tclvalue(stemsValue))
        tkgrab.release(top)
        tkdestroy(top)
        command <- paste("stem(", .activeDataSet, "$", x, ", scale=", stems, ")", sep="")
        logger(command)
        justDoIt(command)
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
        help(stem)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    stemsFrame <- tkframe(top)
    stemsValue <- tclVar("1")
    stemSlider <- tkscale(stemsFrame, from=1, to=4, showvalue=TRUE, variable=stemsValue,
        resolution=1, orient="horizontal")
    tkgrid(tklabel(top, text="Variable (pick one)"), 
        tklabel(top, text="Scale Factor"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(stemSlider, sticky="w")
    tkgrid(xFrame, stemsFrame, sticky="nw")    
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }


boxPlot <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Boxplot")
    xFrame <- tkframe(top)
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    identifyVariable <- tclVar("0")
    identifyFrame <- tkframe(top)
    identifyCheckBox <- tkcheckbutton(identifyFrame, variable=identifyVariable)
    assign(".groups", "FALSE", envir=.GlobalEnv)
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        identifyPoints <- "1" == tclvalue(identifyVariable)
        tkgrab.release(top)
        tkdestroy(top)
        var <- paste(.activeDataSet, "$", x, sep="")
        if (.groups == "FALSE") {
            command <- (paste("boxplot(", var, ', ylab="', x, '")', sep=""))
            logger(command)
            justDoIt(command)     
            if (identifyPoints) doItAndPrint(paste("identify(rep(1, length(", var,
                ")), ", var, ", rownames(", .activeDataSet,"))", sep=""))           
            }
        else {
            command <- (paste("boxplot(", x, "~", .groups, ', ylab="', x, 
                '", xlab="', .groups,'"',
                ", data=", .activeDataSet, ")", sep=""))
            logger(command)
            justDoIt(command)
            if (identifyPoints) doItAndPrint(paste("identify(", .groups, ",", var,
                ", rownames(", .activeDataSet,"))", sep=""))
            }
        tkfocus(.commander)
        }
    onGroups <- function(){
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        groupsFrame <- tkframe(subdialog)
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE",
            yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        onOKsub <- function() {
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=.GlobalEnv)
            tkgrab.release(subdialog)
            tkfocus(top)
            tkgrab(top)
            tkdestroy(subdialog)
            }
        onCancelSub <- function() {
            tkgrab.release(subdialog)  
            tkfocus(top)
            tkgrab(top)
            tkdestroy(subdialog)
            }
        subButtonFrame <- tkframe(subdialog)
        OKSubButton <- tkbutton(subButtonFrame, text="OK", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subButtonFrame, text="Cancel", width="12",command=onCancelSub)
        tkselection.set(groupsBox, 0)
        tkgrid(tklabel(subdialog, text="Groups variable (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid.configure(groupsScroll, sticky="ns")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(subButtonFrame, sticky="w")
        tkbind(subdialog, "<Return>", onOKsub)
        tkfocus(subdialog)
        tkgrab(subdialog)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonFrame, text="OK", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(boxplot)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, text="Plot by groups", command=onGroups)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid(xFrame, sticky="n")    
    tkgrid(tklabel(identifyFrame, text="Identify outliers\nwith mouse"), identifyCheckBox, sticky="w")
    tkgrid(identifyFrame, stick="w")
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

scatterPlot <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Scatterplot")
    xFrame <- tkframe(top)
    yFrame <- tkframe(top)
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    optionsFrame <- tkframe(top)
    identifyVariable <- tclVar("0")
    identifyCheckBox <- tkcheckbutton(optionsFrame, variable=identifyVariable)
    jitterXVariable <- tclVar("0")
    jitterXCheckBox <- tkcheckbutton(optionsFrame, variable=jitterXVariable)
    jitterYVariable <- tclVar("0")
    jitterYCheckBox <- tkcheckbutton(optionsFrame, variable=jitterYVariable)
    boxplots <- tclVar("1")
    boxplotsCheckBox <- tkcheckbutton(optionsFrame, variable=boxplots)
    lsLine <- tclVar("1")
    lsLineCheckBox <- tkcheckbutton(optionsFrame, variable=lsLine)
    smoothLine <- tclVar("1")
    smoothCheckBox <- tkcheckbutton(optionsFrame, variable=smoothLine)
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=1, orient="horizontal")
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable,
        xscrollcommand=function(...) tkset(subsetScroll, ...))
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkyview(subsetEntry, ...))
    assign(".groups", "FALSE", envir=.GlobalEnv)
    assign(".linesByGroup", "FALSE", envir=.GlobalEnv)
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        y <- as.character(tkget(yBox, "active"))
        if (x == y) {
            tkmessageBox(message="x and y variables must be different", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            scatterPlot()
            return()
            }
        if ("1" == tclvalue(jitterXVariable)) x <- paste("jitter(", x, ")", sep="")
        if ("1" == tclvalue(jitterYVariable)) y <- paste("jitter(", y, ")", sep="")
        labels <- if("1" == tclvalue(identifyVariable)) 
            paste("rownames(", .activeDataSet, ")", sep="") else "FALSE"
        box <- if ("1" == tclvalue(boxplots)) "'xy'" else "FALSE"
        line <- if("1" == tclvalue(lsLine)) "lm" else "FALSE"
        smooth <- as.character("1" == tclvalue(smoothLine))
        span <- as.numeric(tclvalue(sliderValue))
        subset <- tclvalue(subsetVariable)
        subset <- if (subset == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)
        if (.groups == "FALSE") {
            doItAndPrint(paste("scatterplot(", y, "~", x,
                ", reg.line=", line, ", smooth=", smooth, ", labels=", labels,
                ", boxplots=", box, ", span =", span/100,
                ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else {
            doItAndPrint(paste("scatterplot(", y, "~", x," | ", .groups,
                ", reg.line=", line, ", smooth=", smooth, ", labels=", labels,
                ", boxplots=", box, ", span=", span/100,
                ", by.groups=", .linesByGroup,
                ", data=", .activeDataSet, subset, ")", sep=""))
            }
        }
    onGroups <- function(){
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        linesByGroupFrame <- tkframe(subdialog)
        linesButtonFrame <- tkframe(subdialog)
        groupsFrame <- tkframe(subdialog)
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE",
            yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        linesByGroup <- tclVar("1")
        linesCheckBox <- tkcheckbutton(linesByGroupFrame, variable=linesByGroup)
        onOKsub <- function(){
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=.GlobalEnv)
            lines <- as.character("1" == tclvalue(linesByGroup))
            assign(".linesByGroup", lines, envir=.GlobalEnv)
            tkgrab.release(subdialog)
            tkfocus(top)
            tkgrab(top)
            tkdestroy(subdialog)
            }
        onCancelSub <- function() {
            tkgrab.release(subdialog)  
            tkfocus(top)
            tkgrab(top)
            tkdestroy(subdialog)
            }
        OKSubButton <- tkbutton(linesButtonFrame, text="OK", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(linesButtonFrame, text="Cancel", width="12", command=onCancelSub)
        tkselection.set(groupsBox, 0)
        tkgrid(tklabel(subdialog, text="Groups (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid.configure(groupsScroll, sticky="ns")
        tkgrid(tklabel(linesByGroupFrame, text="Plot lines by group"), linesCheckBox, sticky="w")
        tkgrid(linesByGroupFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(linesButtonFrame, sticky="w")
        tkgrid(tklabel(subdialog, text="Position legend with mouse click"))
        tkbind(subdialog, "<Return>", onOKsub)
        tkfocus(subdialog)
        tkgrab(subdialog)
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
        help(scatterplot)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, text="Plot by groups", command=onGroups)
    tkgrid(tklabel(top, text="x-variable (pick one)"), 
        tklabel(top, text="y-variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xFrame, yFrame, sticky="w")    
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")
    tkgrid(tklabel(optionsFrame, text="Identify points"), identifyCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Jitter x variable"), jitterXCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Jitter y variable"), jitterYCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Marginal boxplots"), boxplotsCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Least-squares line"), lsLineCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Smooth line"), smoothCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Span for smooth"), slider, sticky="w")
    tkgrid(optionsFrame)
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(xBox, 0)
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

scatterPlotMatrix <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Scatterplot Matrix")
    variablesFrame <- tkframe(top)
    variablesScroll <- tkscrollbar(variablesFrame, 
        repeatinterval=5, command=function(...) tkyview(variablesBox, ...))    
    variablesBox <- tklistbox(variablesFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .numeric) tkinsert(variablesBox, "end", variable)
    optionsFrame <- tkframe(top)
    lsLine <- tclVar("1")
    lsLineCheckBox <- tkcheckbutton(optionsFrame, variable=lsLine)
    smoothLine <- tclVar("1")
    smoothCheckBox <- tkcheckbutton(optionsFrame, variable=smoothLine)
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=1, orient="horizontal")
    diagonalFrame <- tkframe(top)
    diagonal <- tclVar("density")
    histogramButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="histogram")
    densityButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="density")
    boxplotButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="boxplot")
    qqplotButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="qqplot")
    noneButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="none")
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable,
        xscrollcommand=function(...) tkset(subsetScroll, ...))
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkyview(subsetEntry, ...))
    assign(".groups", "FALSE", envir=.GlobalEnv)
    assign(".linesByGroup", "FALSE", envir=.GlobalEnv)
    onOK <- function(){
        variables <- .numeric[as.numeric(tkcurselection(variablesBox)) + 1]
        if (length(variables) < 3) {
            tkmessageBox(message="Fewer than 3 variable selected.", 
                icon="error", type="ok")
            tkdestroy(top)
            scatterPlotMatrix()
            return()
            }
        line <- if("1" == tclvalue(lsLine)) "lm" else "FALSE"
        smooth <- as.character("1" == tclvalue(smoothLine))
        span <- as.numeric(tclvalue(sliderValue))
        diag <- as.character(tclvalue(diagonal))
        subset <- tclvalue(subsetVariable)
        subset <- if (subset == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        tkgrab.release(top)
        tkdestroy(top)
        if (.groups == "FALSE") {
           command <- paste("scatterplot.matrix(~", paste(variables, collapse="+"),
                ", reg.line=", line, ", smooth=", smooth,
                ", span=", span/100, ", diagonal = '", diag,
                "', data=", .activeDataSet, subset, ")", sep="")
           logger(command)
           justDoIt(command)
            }
        else {
            command <- paste("scatterplot.matrix(~", paste(variables, collapse="+")," | ", .groups,
                ", reg.line=", line, ", smooth=", smooth,
                ", span=", span/100, ", diagonal= '", diag,
                "', by.groups=", .linesByGroup,
                ", data=", .activeDataSet, subset, ")", sep="")
            logger(command)
            justDoIt(command)
            }
        tkfocus(.commander)
        }
    onGroups <- function(){
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        linesByGroupFrame <- tkframe(subdialog)
        linesButtonFrame <- tkframe(subdialog)
        groupsFrame <- tkframe(subdialog)
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE",
            yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        linesByGroup <- tclVar("1")
        linesCheckBox <- tkcheckbutton(linesByGroupFrame, variable=linesByGroup)
        onOKsub <- function(){
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=.GlobalEnv)
            lines <- as.character("1" == tclvalue(linesByGroup))
            assign(".linesByGroup", lines, envir=.GlobalEnv)
            tkgrab.release(subdialog)
            tkfocus(top)
            tkgrab(top)
            tkdestroy(subdialog)
            }
        onCancelSub <- function() {
            tkgrab.release(subdialog)  
            tkfocus(top)
            tkgrab(top)
            tkdestroy(subdialog)
            }
        OKSubButton <- tkbutton(linesButtonFrame, text="OK", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(linesButtonFrame, text="Cancel", width="12", command=onCancelSub)
        tkselection.set(groupsBox, 0)
        tkgrid(tklabel(subdialog, text="Groups (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid.configure(groupsScroll, sticky="ns")
        tkgrid(tklabel(linesByGroupFrame, text="Plot lines by group"), linesCheckBox, sticky="w")
        tkgrid(linesByGroupFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(linesButtonFrame, sticky="w")
        tkbind(subdialog, "<Return>", onOKsub)
        tkfocus(subdialog)
        tkgrab(subdialog)
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
        help(scatterplot.matrix)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, text="Plot by groups", command=onGroups)
    tkgrid(tklabel(top, text="Select variables (three or more)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid(variablesFrame, sticky="nw")    
    tkgrid(tklabel(optionsFrame, text="Least-squares line"), lsLineCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Smooth line"), smoothCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Span for smooth"), slider, sticky="w")
    tkgrid(optionsFrame)
    tkgrid(tklabel(diagonalFrame, text="On Diagonal:"), columnspan=2, sticky="w")
    tkgrid(tklabel(diagonalFrame, text="Density plots"), densityButton, sticky="w")
    tkgrid(tklabel(diagonalFrame, text="Histograms"), histogramButton, sticky="w")
    tkgrid(tklabel(diagonalFrame, text="Boxplots"), boxplotButton, sticky="w")
    tkgrid(tklabel(diagonalFrame, text="Normal QQ plots"), qqplotButton, sticky="w")
    tkgrid(tklabel(diagonalFrame, text="Nothing (empty)"), noneButton, sticky="w")
    tkgrid(diagonalFrame, sticky="w")
    tkgrid(tklabel(subsetFrame, text="Subset expression"), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

barGraph <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Bar Graph")
    variableFrame <- tkframe(top)
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .factors) tkinsert(variableBox, "end", var)
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        tkgrab.release(top)
        tkdestroy(top)
        command <- paste("barplot(table(", variable, '), xlab="',
            variable, '", ylab="Frequency")', sep="")
        logger(command)
        justDoIt(command)
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
        help(barplot)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

pieChart <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Pie Chart")
    variableFrame <- tkframe(top)
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .factors) tkinsert(variableBox, "end", var)
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        tkgrab.release(top)
        tkdestroy(top)
        command <- (paste("pie(table(", variable, "), labels=levels(",
            variable, '), main="', variable, '", col=rainbow(length(levels(',
            variable, "))))", sep=""))
        logger(command)
        justDoIt(command)
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
        help(pie)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text="    "), helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }
