# Graphs menu dialogs

# last modified 20 Mar 04 by J. Fox

indexPlot <- function(){
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
    top <- tktoplevel()
    tkwm.title(top, "Index Plot")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        type <- if (tclvalue(typeVariable) == "spikes") "h" else "p"
        identify <- tclvalue(identifyVariable) == "1"
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("plot(", .activeDataSet, "$", x, ', type="', type, '")', sep="")
        doItAndPrint(command)
        if (par("usr")[3] <= 0) doItAndPrint('abline(h=0, col="gray")')
        if (identify) {
            command <- paste("identify(", .activeDataSet, "$", x, 
                ", labels=rownames(", .activeDataSet, "))", sep="")
            doItAndPrint(command)
            }        
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
        help(plot)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    optionsFrame <- tkframe(top)
    typeVariable <- tclVar("spikes")
    spikesButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="spikes")
    pointsButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="points")
    identifyVariable <- tclVar("0")
    identifyCheckBox <- tkcheckbutton(optionsFrame, variable=identifyVariable)
    tkgrid(tklabel(top, text="Variable"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")    
    tkgrid(tklabel(optionsFrame, text="Identify observations\nwith mouse", justify="left"), 
        identifyCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Spikes"), spikesButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Points"), pointsButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame)
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:1) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

Histogram <- function(){
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
    top <- tktoplevel()
    tkwm.title(top, "Histogram")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        bins <- tclvalue(binsVariable)
        bins <- if (bins == "<auto>") '"Sturges"' else as.numeric(bins)
        scale <- tclvalue(scaleVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("Hist(", .activeDataSet, "$", x, ', scale="',
            scale, '", breaks=', bins, ', col="darkgray")', sep="")
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
        help(Hist)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    scaleFrame <- tkframe(top)
    scaleVariable <- tclVar("frequency")
    frequenciesButton <- tkradiobutton(scaleFrame, variable=scaleVariable, value="frequency")
    percentsButton <- tkradiobutton(scaleFrame, variable=scaleVariable, value="percent")
    densitiesButton <- tkradiobutton(scaleFrame, variable=scaleVariable, value="density")
    binsFrame <- tkframe(top)
    binsVariable <- tclVar("<auto>")
    binsField <- tkentry(binsFrame, width="6", textvariable=binsVariable)
    tkgrid(tklabel(top, text="Variable"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")    
    tkgrid(tklabel(scaleFrame, text="Axis Scaling", fg="blue"), columnspan=2, sticky="w")
    tkgrid(tklabel(scaleFrame, text="Frequency counts"), frequenciesButton, sticky="w")
    tkgrid(tklabel(scaleFrame, text="Percentages"), percentsButton, sticky="w")
    tkgrid(tklabel(scaleFrame, text="Densities"), densitiesButton, sticky="w")
    tkgrid(tklabel(binsFrame, text="Number of bins: "), binsField, sticky="w")
    tkgrid(binsFrame, sticky="w")
    tkgrid(scaleFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame)
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(binsField, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

stemAndLeaf <- function(){
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
    top <- tktoplevel()
    tkwm.title(top, "Stem and Leaf Display")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    displayDigits <- tclVar("1")
    onDigits <- function(...){
        tclvalue(displayDigits) <- formatC(10^as.numeric(tclvalue(leafsDigitValue)), 
            format="fg", big.mark=",")
        tclvalue(leafsAutoVariable) <- "0"
        }
    optionsFrame <- tkframe(top)
    partsFrame <- tkframe(top)
    leafsFrame <- tkframe(top)
    styleFrame <- tkframe(top)
    leafsAutoVariable <- tclVar("1")
    leafsDigitCheckBox <- tkcheckbutton(leafsFrame, variable=leafsAutoVariable)
    partsVariable <- tclVar("auto")
    partsAutoButton <- tkradiobutton(partsFrame, variable=partsVariable, value="auto")
    parts1Button <- tkradiobutton(partsFrame, variable=partsVariable, value="1")
    parts2Button <- tkradiobutton(partsFrame, variable=partsVariable, value="2")
    parts5Button <- tkradiobutton(partsFrame, variable=partsVariable, value="5")
    styleVariable <- tclVar("Tukey")
    TukeyButton <- tkradiobutton(styleFrame, variable=styleVariable, value="Tukey")
    bareButton <- tkradiobutton(styleFrame, variable=styleVariable, value="bare")
    trimOutliersVariable <- tclVar("1")
    trimOutliersCheckBox <- tkcheckbutton(optionsFrame, variable=trimOutliersVariable)    
    showDepthsVariable <- tclVar("1")
    showDepthsCheckBox <- tkcheckbutton(optionsFrame, variable=showDepthsVariable)
    reverseNegativeVariable <- tclVar("1")
    reverseNegativeCheckBox <- tkcheckbutton(optionsFrame, variable=reverseNegativeVariable)
    leafsDigitValue <- tclVar("0")
    leafsDigitSlider <- tkscale(leafsFrame, from=-6, to=6, showvalue=FALSE, variable=leafsDigitValue,
        resolution=1, orient="horizontal", command=onDigits)
    leafsDigitShow <- tklabel(leafsFrame, textvariable=displayDigits, width=8, justify="right")
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        unit <- if (tclvalue(leafsAutoVariable) == "1") "" 
            else paste(", unit=", 10^as.numeric(tclvalue(leafsDigitValue)), sep="")
        m <- if (tclvalue(partsVariable) == "auto") ""
            else paste(", m=", tclvalue(partsVariable), sep="")
        trim <- if (tclvalue(trimOutliersVariable) == "1") ""
            else ", trim.outliers=FALSE"
        depths <- if (tclvalue(showDepthsVariable) == "1") ""
            else ", depths=FALSE"
        reverse <- if (tclvalue(reverseNegativeVariable) == "1") ""
            else ", reverse.negative.leaves=FALSE"
        style <- if (tclvalue(styleVariable) == "Tukey") ""
            else ', style="bare"'
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("stem.leaf(", .activeDataSet, "$", x, style, unit, m, trim, 
            depths, reverse, ")", sep="")
        logger(command)
        justDoIt(command)
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
        help(stem.leaf)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")
    tkgrid(tklabel(leafsFrame, text="Leafs Digit:  ", fg="blue"),
        tklabel(leafsFrame, text="Automatic"), leafsDigitCheckBox,
        tklabel(leafsFrame, text="  or set:", fg="red"), leafsDigitShow, leafsDigitSlider, sticky="w")  
    tkgrid(leafsFrame, sticky="w") 
    tkgrid(tklabel(partsFrame, text="Parts Per Stem:  ", fg="blue"),
        tklabel(partsFrame, text="Automatic"), partsAutoButton,
        tklabel(partsFrame, text="  1"), parts1Button, 
        tklabel(partsFrame, text="  2"), parts2Button, 
        tklabel(partsFrame, text="  5"), parts5Button, sticky="w")
    tkgrid(partsFrame, sticky="w")
    tkgrid(tklabel(styleFrame, text="Style of Divided Stems:  ", fg="blue"),
        tklabel(styleFrame, text="Tukey"), TukeyButton,
        tklabel(styleFrame, text="  Repeated stem digits"), bareButton, sticky="w")
    tkgrid(styleFrame, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Trim outliers"), trimOutliersCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Show depths"), showDepthsCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Reverse negative leaves"), reverseNegativeCheckBox, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="        "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tclvalue(leafsAutoVariable) <- "1"
    for (row in 0:6) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

boxPlot <- function(){
    env <- environment()
    .groupsLabel <- tclVar("Plot by groups")
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
    top <- tktoplevel()
    tkwm.title(top, "Boxplot")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    identifyVariable <- tclVar("0")
    identifyFrame <- tkframe(top)
    identifyCheckBox <- tkcheckbutton(identifyFrame, variable=identifyVariable)
    .groups <- FALSE
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        identifyPoints <- "1" == tclvalue(identifyVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        var <- paste(.activeDataSet, "$", x, sep="")
        if (.groups == FALSE) {
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
            if (identifyPoints) doItAndPrint(paste("identify(", .activeDataSet, "$", .groups, ", ", var,
                ", rownames(", .activeDataSet,"))", sep=""))
            }
        tkfocus(.commander)
        }
    onGroups <- function(){
        if (length(.factors) == 0){
            tkmessageBox(message="There no factors in the active data set.", 
                    icon="error", type="ok")
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            return()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        groupsFrame <- tkframe(subdialog)
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE")
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        onOKsub <- function() {
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=env)
            tclvalue(.groupsLabel) <- paste("Plot by:", groups)
            tkconfigure(groupsButton, fg="blue")
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        onCancelSub <- function() {
            assign(".groups", FALSE, envir=env)
            tclvalue(.groupsLabel) <- "Plot by groups"
            tkconfigure(groupsButton, fg="black")
            if (.grab.focus) tkgrab.release(subdialog) 
            tkdestroy(subdialog) 
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        subButtonFrame <- tkframe(subdialog)
        OKSubButton <- tkbutton(subButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subButtonFrame, text="Cancel", fg="red", width="12",command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Groups variable (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(subButtonFrame, sticky="w")
        tkgrid.configure(groupsScroll, sticky="ns")
        for (row in 0:2) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkbind(groupsBox, "<Double-ButtonPress-1>", onOKsub)
        tkselection.set(groupsBox, 0)
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(boxplot)
        }
    helpButton <- tkbutton(buttonFrame, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, textvariable=.groupsLabel, command=onGroups)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="w")    
    tkgrid(tklabel(identifyFrame, text="Identify outliers\nwith mouse", justify="left"), 
        identifyCheckBox, sticky="w")
    tkgrid(identifyFrame, stick="w")
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonFrame, text="    "), helpButton,sticky="w")
    tkgrid(buttonFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

scatterPlot <- function(){
    env <- environment()
    .groupsLabel <- tclVar("Plot by groups")
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
    tkwm.title(top, "Scatterplot")
    xFrame <- tkframe(top)
    yFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    tkconfigure(yBox, yscrollcommand=function(...) tkset(yScroll, ...))
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
        resolution=5, orient="horizontal")
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    .groups <- FALSE
    .linesByGroup <- FALSE
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        y <- as.character(tkget(yBox, "active"))
        if (x == y) {
            tkmessageBox(message="x and y variables must be different", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
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
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)
        if (.groups == FALSE) {
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
        if (length(.factors) == 0){
            tkmessageBox(message="There are no factors in the active data set.", 
                    icon="error", type="ok")
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            return()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        linesByGroupFrame <- tkframe(subdialog)
        linesButtonFrame <- tkframe(subdialog)
        groupsFrame <- tkframe(subdialog)
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE")
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        linesByGroup <- tclVar("1")
        linesCheckBox <- tkcheckbutton(linesByGroupFrame, variable=linesByGroup)
        onOKsub <- function(){
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=env)
            tclvalue(.groupsLabel) <- paste("Plot by:", groups)
            tkconfigure(groupsButton, fg="blue")
            lines <- as.character("1" == tclvalue(linesByGroup))
            assign(".linesByGroup", lines, envir=env)
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        onCancelSub <- function() {
            assign(".groups", FALSE, envir=env)
            assign(".linesByGroup", FALSE, envir=env)
            tclvalue(.groupsLabel) <- "Plot by groups"
            tkconfigure(groupsButton, fg="black")
            if (.grab.focus) tkgrab.release(subdialog)  
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        OKSubButton <- tkbutton(linesButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(linesButtonFrame, text="Cancel", fg="red", width="12", command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Groups (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid(tklabel(linesByGroupFrame, text="Plot lines by group"), linesCheckBox, sticky="w")
        tkgrid(linesByGroupFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(linesButtonFrame, sticky="w")
        tkgrid(tklabel(subdialog, text="Position legend with mouse click", fg="blue"))
        tkgrid.configure(groupsScroll, sticky="ns")
        for (row in 0:4) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkselection.set(groupsBox, 0)
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
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
        help(scatterplot)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, textvariable=.groupsLabel, command=onGroups)
    tkgrid(tklabel(top, text="x-variable (pick one)"), 
        tklabel(top, text="y-variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xFrame, yFrame, sticky="w")    
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
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:5) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

scatterPlotMatrix <- function(){
    env <- environment()
    .groupsLabel <- tclVar("Plot by groups")
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 3){
        tkmessageBox(message="There are fewer than 3 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Scatterplot Matrix")
    variablesFrame <- tkframe(top)
    variablesBox <- tklistbox(variablesFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    variablesScroll <- tkscrollbar(variablesFrame, 
        repeatinterval=5, command=function(...) tkyview(variablesBox, ...))    
    tkconfigure(variablesBox, yscrollcommand=function(...) tkset(variablesScroll, ...))
    for (variable in .numeric) tkinsert(variablesBox, "end", variable)
    optionsFrame <- tkframe(top)
    lsLine <- tclVar("1")
    lsLineCheckBox <- tkcheckbutton(optionsFrame, variable=lsLine)
    smoothLine <- tclVar("1")
    smoothCheckBox <- tkcheckbutton(optionsFrame, variable=smoothLine)
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=5, orient="horizontal")
    diagonalFrame <- tkframe(top)
    diagonal <- tclVar("density")
    histogramButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="histogram")
    densityButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="density")
    boxplotButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="boxplot")
    qqplotButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="qqplot")
    noneButton <- tkradiobutton(diagonalFrame, variable=diagonal, value="none")
    subsetVariable <- tclVar("<all valid cases>")
    subsetFrame <- tkframe(top)
    subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
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
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" 
            else paste(", subset=", subset, sep="")
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        if (.groups == FALSE) {
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
        if (length(.factors) == 0){
            tkmessageBox(message="There are no factors in the active data set.", 
                    icon="error", type="ok")
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            return()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        linesByGroupFrame <- tkframe(subdialog)
        linesButtonFrame <- tkframe(subdialog)
        groupsFrame <- tkframe(subdialog)
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE")
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        linesByGroup <- tclVar("1")
        linesCheckBox <- tkcheckbutton(linesByGroupFrame, variable=linesByGroup)
        onOKsub <- function(){
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=env)
            tclvalue(.groupsLabel) <- paste("Plot by:", groups)
            tkconfigure(groupsButton, fg="blue")
            lines <- as.character("1" == tclvalue(linesByGroup))
            assign(".linesByGroup", lines, envir=env)
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        onCancelSub <- function() {
            assign(".groups", FALSE, envir=env)
            tclvalue(.groupsLabel) <- "Plot by groups"
            tkconfigure(groupsButton, fg="black")
            if (.grab.focus) tkgrab.release(subdialog)  
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        OKSubButton <- tkbutton(linesButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(linesButtonFrame, text="Cancel", fg="red", width="12", command=onCancelSub)
        tkselection.set(groupsBox, 0)
        tkgrid(tklabel(subdialog, text="Groups (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid(tklabel(linesByGroupFrame, text="Plot lines by group"), linesCheckBox, sticky="w")
        tkgrid(linesByGroupFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(linesButtonFrame, sticky="w")
        tkgrid.configure(groupsScroll, sticky="ns")
        for (row in 0:3) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
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
        help(scatterplot.matrix)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, textvariable=.groupsLabel, command=onGroups)
    tkgrid(tklabel(top, text="Select variables (three or more)"), sticky="w")
    tkgrid(variablesBox, variablesScroll, sticky="nw")
    tkgrid(variablesFrame, sticky="nw")    
    tkgrid(tklabel(optionsFrame, text="Least-squares line"), lsLineCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Smooth line"), smoothCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Span for smooth"), slider, sticky="w")
    tkgrid(optionsFrame)
    tkgrid(tklabel(diagonalFrame, text="On Diagonal:", fg="blue"), columnspan=2, sticky="w")
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
    for (row in 0:6) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkgrid.configure(variablesScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

barGraph <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There are no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Bar Graph")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .factors) tkinsert(variableBox, "end", var)
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("barplot(table(", .activeDataSet, "$", variable, '), xlab="',
            variable, '", ylab="Frequency")', sep="")
        logger(command)
        justDoIt(command)
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
        help(barplot)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(variableBox, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

pieChart <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.factors) == 0){
        tkmessageBox(message="There are no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Pie Chart")
    variableFrame <- tkframe(top)
    variableBox <- tklistbox(variableFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    variableScroll <- tkscrollbar(variableFrame, repeatinterval=5, 
        command=function(...) tkyview(variableBox, ...))
    tkconfigure(variableBox, yscrollcommand=function(...) tkset(variableScroll, ...))
    for (var in .factors) tkinsert(variableBox, "end", var)
    onOK <- function(){
        variable <- as.character(tkget(variableBox, "active"))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- (paste("pie(table(", .activeDataSet, "$", variable, "), labels=levels(",
            .activeDataSet, "$", variable, '), main="', variable, '", col=rainbow(length(levels(',
            .activeDataSet, "$", variable, "))))", sep=""))
        logger(command)
        justDoIt(command)
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
        help(pie)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(variableBox, variableScroll, sticky="nw")
    tkgrid(variableFrame, sticky="w")
    tkgrid.configure(variableScroll, sticky="ns")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(variableBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkbind(variableBox, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

linePlot <- function(){
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
    tkwm.title(top, "Line Plot")
    variablesFrame <- tkframe(top)
    xFrame <- tkframe(variablesFrame)
    yFrame <- tkframe(variablesFrame)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="multiple", background="white", exportselection="FALSE")
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    tkconfigure(yBox, yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    axisLabelVariable <- tclVar("<use y-variable names>")
    axisLabelFrame <- tkframe(top)
    axisLabelEntry <- tkentry(axisLabelFrame, width="40", textvariable=axisLabelVariable)
    axisLabelScroll <- tkscrollbar(axisLabelFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(axisLabelEntry, ...))
    tkconfigure(axisLabelEntry, xscrollcommand=function(...) tkset(axisLabelScroll, ...))
    legendFrame <- tkframe(top)
    legendVariable <- tclVar("0")
    legendCheckBox <- tkcheckbutton(legendFrame, variable=legendVariable)
    onOK <- function(){
        y <- .numeric[as.numeric(tkcurselection(yBox)) + 1]
        x <- as.character(tkget(xBox, "active"))
        if (0 == length(y)) {
            tkmessageBox(message="No y variables selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linePlot()
            return()
            }
        if (is.element(x, y)) {
            tkmessageBox(message="x and y variables must be different.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            linePlot()
            return()
            }
        .x <- na.omit(eval(parse(text=paste(.activeDataSet, "$", x, sep="")), envir=.GlobalEnv))
        if (!identical(order(.x), seq(along=.x))){
            response <- tclvalue(tkmessageBox(message="x-values are not in order.\nContinue?", 
                icon="warning", type="okcancel", default="cancel"))
            if (response == "cancel") {
                onCancel()
                return()
                }
            }
        axisLabel <- tclvalue(axisLabelVariable)
        legend <- tclvalue(legendVariable) == "1"
        if (axisLabel == "<use y-variable names>"){
            axisLabel <- if (legend) ""
                else if(length(y) == 1) y
                else paste(paste("(", 1:length(y), ") ", y, sep=""), collapse=", ")
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        pch <- if (length(y) == 1) ", pch=1" else ""
        command <- paste("matplot(", .activeDataSet, "$", x, ", ", .activeDataSet, "[, ",
            paste("c(", paste(paste('"', y, '"', sep=""), collapse=","), ")", sep=""),
            '], type="b", lty=1, ylab="', axisLabel, '"', pch, ")", sep="")
        logger(command)
        justDoIt(command)
        if (legend && length(y) > 1){
            n <- length(y)
            cols <- rep(1:6, 1 + n %/% 6)[1:n]
            command <- paste("legend(locator(1), legend=", 
                paste("c(", paste(paste('"', y, '"', sep=""), collapse=","), ")", sep=""),
                ", col=c(", paste(cols, collapse=","), "), lty=1, pch=c(",
                paste(paste('"', as.character(1:n), '"', sep=""), collapse=","), "))", sep="")
            logger(command)
            justDoIt(command)
            }
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
        help(matplot)
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(variablesFrame, text="x variable (pick one)"), 
        tklabel(variablesFrame, text="    "),
        tklabel(variablesFrame, text="y variables (pick one or more)"), sticky="w")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, tklabel(variablesFrame, text="    "), yFrame, sticky="nw")
    tkgrid(variablesFrame, sticky="w")    
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(tklabel(axisLabelFrame, text="Label for y-axis"), sticky="w")
    tkgrid(axisLabelEntry, sticky="w")
    tkgrid(axisLabelScroll, sticky="ew")
    tkgrid(axisLabelFrame, sticky="w")
    tkgrid(tklabel(legendFrame, text="Plot legend (position with mouse click)"),
        legendCheckBox, sticky="w")
    tkgrid(legendFrame, sticky="w")
    tkgrid(buttonsFrame, stick="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
#    if (.grab.focus) tkgrab.set(top)  # causes problem ?
    tkfocus(top)
    tkwait.window(top)
    }
    
QQPlot <- function()
# this function modified by Martin Maechler
{
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
    top <- tktoplevel()
    tkwm.title(top, "Quantile-Comparison (QQ) Plot")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
                      selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        dist <- tclvalue(distVariable)
        save <- options(warn=-1)
        on.exit(options=save)
        retryMe <- function(msg) {
            tkmessageBox(message= msg, icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            QQPlot()
        }
        switch(dist,
               "norm" = { args <- 'dist= "norm"' },
               "t" =  {
                   df <- tclvalue(tDfVariable)
                   df.num <- as.numeric(df)
                   if (is.na(df.num) || df.num < 1) {
                       retryMe("df for t must be a positive number.")
                       return()
                   }
                   args <- paste('dist="t", df=', df, sep="")
               },
               "chisq" = {
                   df <- tclvalue(chisqDfVariable)
                   df.num <- as.numeric(df)
                   if (is.na(df.num) || df.num < 1) {
                       retryMe("df for chi-square must be a positive number.")
                       return()
                   }
                   args <- paste('dist="chisq", df=', df, sep="")
               },
               "f" = {
                   df1 <- tclvalue(FDf1Variable)
                   df2 <- tclvalue(FDf2Variable)
                   df.num1 <- as.numeric(df1)
                   df.num2 <- as.numeric(df2)
                   if (is.na(df.num1) || df.num1 < 1 ||
                       is.na(df.num2) || df.num2 < 1) {
                       retryMe("numerator and denominator \ndf for F must be positive numbers.")
                       return()
                   }
                   args <- paste('dist="f", df1=', df1, ', df2=', df2, sep="")
               },
               ## else -- other `dist' :
           {
               dist <- tclvalue(otherNameVariable)
               params <- tclvalue(otherParamsVariable)
               args <- paste('dist="', dist,'", ', params, sep="")
           }) # end{switch}
        labels <-
            if ("1" == tclvalue(identifyVariable))
                paste("rownames(", .activeDataSet, ")", sep="")
            else "FALSE"
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        command <- paste("qq.plot", "(", .activeDataSet, "$", x, ", ", args,
                          ", labels=", labels, ")", sep="")
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
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",
                             command=onCancel)
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12",
                           command=function()help(qq.plot))
    distFrame <- tkframe(top)
    distVariable <- tclVar("norm")
    normalButton <- tkradiobutton(distFrame, variable=distVariable, value="norm")
    tButton <- tkradiobutton(distFrame, variable=distVariable, value="t")
    chisqButton <- tkradiobutton(distFrame, variable=distVariable, value="chisq")
    FButton <- tkradiobutton(distFrame, variable=distVariable, value="f")
    otherButton <- tkradiobutton(distFrame, variable=distVariable, value="other")
    tDfFrame <- tkframe(distFrame)
    tDfVariable <- tclVar("")
    tDfField <- tkentry(tDfFrame, width="6", textvariable=tDfVariable)
    chisqDfFrame <- tkframe(distFrame)
    chisqDfVariable <- tclVar("")
    chisqDfField <- tkentry(chisqDfFrame, width="6", textvariable=chisqDfVariable)
    FDfFrame <- tkframe(distFrame)
    FDf1Variable <- tclVar("")
    FDf1Field <- tkentry(FDfFrame, width="6", textvariable=FDf1Variable)
    FDf2Variable <- tclVar("")
    FDf2Field <- tkentry(FDfFrame, width="6", textvariable=FDf2Variable)
    otherParamsFrame <- tkframe(distFrame)
    otherParamsVariable <- tclVar("")
    otherParamsField <- tkentry(otherParamsFrame, width="30", textvariable=otherParamsVariable)
    otherNameVariable <- tclVar("")
    otherNameField <- tkentry(otherParamsFrame, width="10", textvariable=otherNameVariable)
    identifyVariable <- tclVar("0")
    identifyFrame <- tkframe(top)
    identifyCheckBox <- tkcheckbutton(identifyFrame, variable=identifyVariable)
    tkgrid(tklabel(top, text="Variable"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")
    tkgrid(tklabel(identifyFrame, text="Identify observations\nwith mouse", justify="left"),
           identifyCheckBox, sticky="w")
    tkgrid(identifyFrame, sticky="w")
    tkgrid(tklabel(distFrame, text="Distribution", fg="blue"), columnspan=6, sticky="w")
    tkgrid(tklabel(distFrame, text="Normal"), normalButton, sticky="w")
    tkgrid(tklabel(tDfFrame, text="df = "), tDfField, sticky="w")
    tkgrid(tklabel(distFrame, text="t"), tButton, tDfFrame, sticky="w")
    tkgrid(tklabel(chisqDfFrame, text="df = "), chisqDfField, sticky="w")
    tkgrid(tklabel(distFrame, text="Chi-square"), chisqButton,
           chisqDfFrame, sticky="w")
    tkgrid(tklabel(FDfFrame, text="Numerator df = "), FDf1Field,
           tklabel(FDfFrame, text="Denominator df = "), FDf2Field, sticky="w")
    tkgrid(tklabel(distFrame, text="F"), FButton, FDfFrame, sticky="w")
    tkgrid(tklabel(otherParamsFrame, text="Specify: "),
           otherNameField, tklabel(otherParamsFrame, text="Parameters: "),
           otherParamsField, sticky="w")
    tkgrid(tklabel(distFrame, text="Other"), otherButton,
           otherParamsFrame, sticky="w")
    tkgrid(distFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="    "), helpButton, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

PlotMeans <- function(){
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
    if (length(.factors) == 0){
        tkmessageBox(message="There no factors in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "Plot Means")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="multiple", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        groups <- .factors[as.numeric(tkcurselection(groupBox)) + 1]
        response <- as.character(tkget(responseBox, "active"))
        if (0 == length(groups)) {
            tkmessageBox(message="No factors selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            PlotMeans()
            return()
            }
        if (2 < length(groups)) {
            tkmessageBox(message="More than two factors selected.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            PlotMeans()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        error.bars <- tclvalue(errorBarsVariable)
        level <- if (error.bars == "conf.int") paste(", level=", tclvalue(levelVariable), sep="") else ""
        if (length(groups) == 1) doItAndPrint(paste("plotMeans(", .activeDataSet, "$", response, 
            ", ", .activeDataSet, "$", groups[1], 
            ', error.bars="', error.bars, '"', level, ')', sep=""))
        else {
            if (eval(parse(text=paste("length(levels(", .activeDataSet, "$", groups[1], 
                ")) < length(levels(", .activeDataSet, "$", groups[2], "))", sep=""))))
                groups <- rev(groups)
            doItAndPrint(paste("plotMeans(", .activeDataSet, "$", response, ", ", .activeDataSet, "$", groups[1], 
                ", ", .activeDataSet, "$", groups[2], ', error.bars="', error.bars, '"', level, ')', sep=""))
            }
        tkfocus(.commander)
        }
    optionsFrame <- tkframe(top)
    errorBarsVariable <- tclVar("se")
    seButton <- tkradiobutton(optionsFrame, variable=errorBarsVariable, value="se")
    sdButton <- tkradiobutton(optionsFrame, variable=errorBarsVariable, value="sd")
    confIntButton <- tkradiobutton(optionsFrame, variable=errorBarsVariable, value="conf.int")
    noneButton <- tkradiobutton(optionsFrame, variable=errorBarsVariable, value="none")
    levelVariable <- tclVar("0.95")
    levelEntry <- tkentry(optionsFrame, width="6", textvariable=levelVariable)    
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
        help(plotMeans)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Factors (pick one or two)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(tklabel(optionsFrame, text="Error Bars", fg="blue"), sticky="w")
    tkgrid(tklabel(optionsFrame, text="Standard errors"), seButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Standard deviations"), sdButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Confidence intervals"), confIntButton,
        tklabel(optionsFrame, text="   Level of confidence:"), levelEntry, sticky="w")
    tkgrid(tklabel(optionsFrame, text="No error bars"), noneButton, sticky="w")
    tkgrid(optionsFrame, sticky="w", columnspan=2)
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
    tkwm.deiconify(top)
    if (.grab.focus) tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

Scatter3D <- function(){
    env <- environment()
    .groupsLabel <- tclVar("Plot by groups")
    rgl <- require(rgl)
    mgcv <- require(mgcv)
    absent <- !c(rgl, mgcv)
    if (any(absent)) {
        tkmessageBox(message= paste(
            "The following packages required for 3D scatterplots are missing:\n",
            paste(c("rgl", "mgcv")[absent], collapse=", ")), icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        return()
        }
    if (length(.numeric) < 3){
        tkmessageBox(message="There are fewer than 3 numeric variables in the active data set.", 
                icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    top <- tktoplevel()
    tkwm.title(top, "3D Scatterplot")
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
    surfacesFrame <- tkframe(top)
    gridLines <- tclVar("1")
    gridLinesCheckBox <- tkcheckbutton(surfacesFrame, variable=gridLines)
    linearLSSurface <- tclVar("1")
    linearLSCheckBox <- tkcheckbutton(surfacesFrame, variable=linearLSSurface)
    quadLSSurface <- tclVar("0")
    quadLSCheckBox <- tkcheckbutton(surfacesFrame, variable=quadLSSurface)
    nonparSurface <- tclVar("0")
    nonparCheckBox <- tkcheckbutton(surfacesFrame, variable=nonparSurface)
    dfNonparVariable <- tclVar("<auto>")
    dfNonparField <- tkentry(surfacesFrame, width="6", textvariable=dfNonparVariable)
    additiveSurface <- tclVar("0")
    additiveCheckBox <- tkcheckbutton(surfacesFrame, variable=additiveSurface)
    dfAddVariable <- tclVar("<auto>")
    dfAddField <- tkentry(surfacesFrame, width="6", textvariable=dfAddVariable)
    bgFrame <- tkframe(top)
    bgVariable <-tclVar("black")
    whiteButton <- tkradiobutton(bgFrame, variable=bgVariable, value="white")
    blackButton <- tkradiobutton(bgFrame, variable=bgVariable, value="black")
    assign(".groups", FALSE, envir=env)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        y <- as.character(tkget(yBox, "active"))
        if (2 != length(x)) {
            tkmessageBox(message="You must select 2 explanatory variables.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Scatter3D()
            return()
            }
        if (is.element(y, x)) {
            tkmessageBox(message="Response and explanatory variables must be different.", 
                icon="error", type="ok")
            if (.grab.focus) tkgrab.release(top)
            tkdestroy(top)
            Scatter3D()
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        grid <- if (tclvalue(gridLines) == 1) "TRUE" else "FALSE"
        lin <- if(tclvalue(linearLSSurface) == 1) '"linear"'
        quad <- if(tclvalue(quadLSSurface) == 1) '"quadratic"'
        nonpar <- if (tclvalue(nonparSurface) == 1) '"smooth"'
        additive <- if (tclvalue(additiveSurface) == 1) '"additive"'
        surfaces <- c(lin, quad, nonpar, additive)
        nsurfaces <- length(surfaces)
        dfNonpar <- tclvalue(dfNonparVariable)
        dfNonpar <- if (dfNonpar == "<auto>") "" else paste(", df.smooth=", as.numeric(dfNonpar), sep="")
        dfAdd <- tclvalue(dfAddVariable)
        dfAdd <- if (dfAdd == "<auto>") "" else paste(", df.additive=", as.numeric(dfAdd), sep="")
        fit <- if (nsurfaces == 0) ", surface=FALSE"
            else if (nsurfaces == 1) paste(", fit=", surfaces, sep="")
            else paste(", fit=c(", paste(surfaces, collapse=","), ")", sep="")
        bg <- tclvalue(bgVariable)
        if (.groups != FALSE){ 
            groups <- paste(", groups=", .groups, sep="")
            parallel <- paste(", parallel=", .parallelSurfaces, sep="")
            }
        else groups <- parallel <- ""                   
        command <- paste("scatter3d(", x[1], ", ", y, ", ", x[2], fit, dfNonpar, 
            dfAdd, groups, parallel, ', bg="', bg, '", grid=', grid, ')', sep="")
        doItAndPrint(command)
        assign(".rgl", TRUE, envir=.GlobalEnv)
        tkfocus(.commander)
        }
    onGroups <- function(){
        if (length(.factors) == 0){
            tkmessageBox(message="There no factors in the active data set.", 
                    icon="error", type="ok")
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            return()
            }
        subdialog <- tktoplevel()
        tkwm.title(subdialog, "Groups")
        groupsFrame <- tkframe(subdialog)
        groupsBox <- tklistbox(groupsFrame, height=min(4, length(.factors)),
            selectmode="single", background="white", exportselection="FALSE")
        groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(groupsBox, ...))
        tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
        for (groups in .factors) tkinsert(groupsBox, "end", groups)
        parallelSurfacesFrame <- tkframe(subdialog)
        parallelSurfacesVariable <- tclVar("1")
        parallelSurfacesCheckBox <- tkcheckbutton(parallelSurfacesFrame, variable=parallelSurfacesVariable)
        onOKsub <- function() {
            groups <- as.character(tkget(groupsBox, "active"))
            assign(".groups", groups, envir=env)
            tclvalue(.groupsLabel) <- paste("Plot by:", groups)
            tkconfigure(groupsButton, fg="blue")
            assign(".parallelSurfaces", tclvalue(parallelSurfacesVariable) == "1", envir=env)
            if (.grab.focus) tkgrab.release(subdialog)
            tkdestroy(subdialog)
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        onCancelSub <- function() {
            assign(".groups", FALSE, envir=env)
            tclvalue(.groupsLabel) <- "Plot by groups"
            tkconfigure(groupsButton, fg="black")
            if (.grab.focus) tkgrab.release(subdialog) 
            tkdestroy(subdialog) 
            tkwm.deiconify(top)
            if (.grab.focus) tkgrab.set(top)
            tkfocus(top)
            tkwait.window(top)
            }
        subButtonFrame <- tkframe(subdialog)
        OKSubButton <- tkbutton(subButtonFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active")
        cancelSubButton <- tkbutton(subButtonFrame, text="Cancel", fg="red", width="12",command=onCancelSub)
        tkgrid(tklabel(subdialog, text="Groups variable (pick one)"), sticky="w")
        tkgrid(groupsBox, groupsScroll, sticky="nw")
        tkgrid(groupsFrame, sticky="w")
        tkgrid(tklabel(parallelSurfacesFrame, text="Parallel regression surfaces"), parallelSurfacesCheckBox, sticky="w")
        tkgrid(parallelSurfacesFrame, sticky="w")
        tkgrid(OKSubButton, cancelSubButton, sticky="w")
        tkgrid(subButtonFrame, sticky="w")
        tkgrid.configure(groupsScroll, sticky="ns")
        for (row in 0:3) tkgrid.rowconfigure(subdialog, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(subdialog, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(subdialog, 0, 0)
        tkbind(subdialog, "<Return>", onOKsub)
        if (.double.click) tkbind(subdialog, "<Double-ButtonPress-1>", onOKsub)
        tkbind(groupsBox, "<Double-ButtonPress-1>", onOKsub)
        tkselection.set(groupsBox, 0)
        tkwm.deiconify(subdialog)
        if (.grab.focus) tkgrab.set(subdialog)
        tkfocus(subdialog)
        tkwait.window(subdialog)
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
        help("Scatter3DDialog")
        }
    helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp)
    groupsButton <- tkbutton(top, textvariable=.groupsLabel, command=onGroups)
    tkgrid(tklabel(variablesFrame, text="Response variable (pick one)"), 
        tklabel(variablesFrame, text="    "),
        tklabel(variablesFrame, text="Explanatory variables (pick two)"), sticky="w")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yFrame, tklabel(variablesFrame, text="    "), xFrame, sticky="nw")
    tkgrid(variablesFrame, sticky="w")   
    tkgrid(tklabel(surfacesFrame, text="Show surface grid lines"), gridLinesCheckBox, sticky="w")
    tkgrid(tklabel(surfacesFrame, text="Surfaces to Fit", fg="blue"), sticky="w")
    tkgrid(tklabel(surfacesFrame, text="Linear least-squares"), linearLSCheckBox, sticky="w")
    tkgrid(tklabel(surfacesFrame, text="Quadratic least-squares"), quadLSCheckBox, sticky="w")
    dfLabel <- tklabel(surfacesFrame, text="df = ")
    tkgrid(tklabel(surfacesFrame, text="Smooth regression"), nonparCheckBox, 
        dfLabel, dfNonparField, sticky="w")
    tkgrid.configure(dfLabel, sticky="e")
    tkgrid(tklabel(surfacesFrame, text="Additive regression"), additiveCheckBox, 
        tklabel(surfacesFrame, text="df(each term) = "), dfAddField, sticky="w")
    tkgrid(surfacesFrame, sticky="w") 
    tkgrid(tklabel(bgFrame, text="Background Color", fg="blue"), sticky="w", columnspan=2)
    tkgrid(tklabel(bgFrame, text="Black"), blackButton, sticky="w")
    tkgrid(tklabel(bgFrame, text="White"), whiteButton, sticky="w")
    tkgrid(bgFrame, sticky="w")
    tkgrid(groupsButton, sticky="w")
    tkgrid(OKbutton, cancelButton, tklabel(buttonsFrame, text="            "), 
        helpButton, sticky="w")
    tkgrid(buttonsFrame, stick="w")
    tkgrid.configure(helpButton, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
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
