# Graphs menu dialogs

# last modified 21 August 06 by J. Fox

indexPlot <- function(){
    initializeDialog(title=gettextRcmdr("Index Plot"))                                               
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(xBox)
        closeDialog()
        if (length(x) == 0){
            errorCondition(recall=indexPlot, message=gettextRcmdr("You must select a variable"))
            return()
            }
        type <- if (tclvalue(typeVariable) == "spikes") "h" else "p"
        identify <- tclvalue(identifyVariable) == "1"
        .activeDataSet <- ActiveDataSet()
        command <- paste("plot(", .activeDataSet, "$", x, ', type="', type, '")', sep="")
        doItAndPrint(command)
        if (par("usr")[3] <= 0) doItAndPrint('abline(h=0, col="gray")')
        if (identify) {
            RcmdrTkmessageBox(title="Identify Points",
                message=gettextRcmdr("Use left mouse button to identify points,\nright button to exit."),
                icon="info", type="ok")
            command <- paste("identify(", .activeDataSet, "$", x, 
                ", labels=rownames(", .activeDataSet, "))", sep="")
            doItAndPrint(command)
            }
        activateMenus()        
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="plot")
    optionsFrame <- tkframe(top)
    typeVariable <- tclVar("spikes")
    spikesButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="spikes")
    pointsButton <- tkradiobutton(optionsFrame, variable=typeVariable, value="points")
    identifyVariable <- tclVar("0")
    identifyCheckBox <- tkcheckbutton(optionsFrame, variable=identifyVariable)
    tkgrid(getFrame(xBox), sticky="nw")    
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Identify observations\nwith mouse"), justify="left"), 
        identifyCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Spikes")), spikesButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Points")), pointsButton, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }

Histogram <- function(){
    initializeDialog(title=gettextRcmdr("Histogram"))
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(xBox)
        closeDialog()
        if (length(x) == 0){
            errorCondition(recall=Histogram, message=gettextRcmdr("You must select a variable"))
            return()
            }
        bins <- tclvalue(binsVariable)
        opts <- options(warn=-1)
        bins <- if (bins == gettextRcmdr("<auto>")) '"Sturges"' else as.numeric(bins)
        options(opts)
        scale <- tclvalue(scaleVariable)
        command <- paste("Hist(", ActiveDataSet(), "$", x, ', scale="',
            scale, '", breaks=', bins, ', col="darkgray")', sep="")
        doItAndPrint(command)
        activateMenus()
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="Hist")
    radioButtons(name="scale", buttons=c("frequency", "percent", "density"),
        labels=gettextRcmdr(c("Frequency counts", "Percentages", "Densities")), title=gettextRcmdr("Axis Scaling"))
    binsFrame <- tkframe(top)
    binsVariable <- tclVar(gettextRcmdr("<auto>"))
    binsField <- tkentry(binsFrame, width="6", textvariable=binsVariable)
    tkgrid(getFrame(xBox), sticky="nw")    
    tkgrid(tklabel(binsFrame, text=gettextRcmdr("Number of bins: ")), binsField, sticky="w")
    tkgrid(binsFrame, sticky="w")
    tkgrid(scaleFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(binsField, sticky="e")
    dialogSuffix(rows=4, columns=1)
    }

stemAndLeaf <- function(){
    initializeDialog(title=gettextRcmdr("Stem and Leaf Display"), preventCrisp=TRUE)
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
    displayDigits <- tclVar("1")
    onDigits <- function(...){
        tclvalue(displayDigits) <- formatC(10^as.numeric(tclvalue(leafsDigitValue)), 
            format="fg", big.mark=",")
        tclvalue(leafsAutoVariable) <- "0"
        }
    radioButtons(name="parts", buttons=c("auto", "one", "two", "five"), 
        values=c("auto", "1", "2", "5"), labels=c(gettextRcmdr("Automatic"), "   1", "   2", "   5"),
        title=gettextRcmdr("Parts Per Stem"))
    radioButtons(name="style", buttons=c("Tukey", "bare"), labels=gettextRcmdr(c("Tukey", "Repeated stem digits")),
        title=gettextRcmdr("Style of Divided Stems"))
    checkBoxes(frame="optionsFrame", boxes=c("trimOutliers", "showDepths", "reverseNegative"), 
        initialValues=rep(1, 3), labels=gettextRcmdr(c("Trim outliers", "Show depths", "Reverse negative leaves")))
    leafsFrame <- tkframe(top)
    leafsDigitValue <- tclVar("0")
    leafsDigitSlider <- tkscale(leafsFrame, from=-6, to=6, showvalue=FALSE, variable=leafsDigitValue,
        resolution=1, orient="horizontal", command=onDigits)
    leafsDigitShow <- tklabel(leafsFrame, textvariable=displayDigits, width=8, justify="right")
    leafsAutoVariable <- tclVar("1")
    leafsDigitCheckBox <- tkcheckbutton(leafsFrame, variable=leafsAutoVariable)
    onOK <- function(){
        x <- getSelection(xBox)
        closeDialog()
        if (length(x) == 0){
            errorCondition(recall=stemAndLeaf, message=gettextRcmdr("You must select a variable"))
            return()
            }
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
        command <- paste("stem.leaf(", ActiveDataSet(), "$", x, style, unit, m, trim,
            depths, reverse, ")", sep="")
        doItAndPrint(command)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="stem.leaf")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(leafsFrame, text=gettextRcmdr("Leafs Digit:  "), fg="blue"),
        tklabel(leafsFrame, text=gettextRcmdr("Automatic")), leafsDigitCheckBox,
        tklabel(leafsFrame, text=gettextRcmdr("  or set:"), fg="red"), leafsDigitShow, leafsDigitSlider, sticky="w")  
    tkgrid(leafsFrame, sticky="w") 
    tkgrid(partsFrame, sticky="w")
    tkgrid(styleFrame, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Options"), fg="blue"), sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tclvalue(leafsAutoVariable) <- "1"
    dialogSuffix(rows=7, columns=1, preventCrisp=TRUE)
    }

boxPlot <- function(){
    initializeDialog(title=gettextRcmdr("Boxplot"))
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
    identifyVariable <- tclVar("0")
    identifyFrame <- tkframe(top)
    identifyCheckBox <- tkcheckbutton(identifyFrame, variable=identifyVariable)
    .groups <- FALSE
    onOK <- function(){
        x <- getSelection(xBox)
        closeDialog()
        if (length(x) == 0){
            errorCondition(recall=boxPlot, message=gettextRcmdr("You must select a variable"))
            return()
            }
        identifyPoints <- "1" == tclvalue(identifyVariable)
        .activeDataSet <- ActiveDataSet()
        var <- paste(.activeDataSet, "$", x, sep="")
        if (.groups == FALSE) {
            command <- (paste("boxplot(", var, ', ylab="', x, '")', sep=""))
            logger(command)
            justDoIt(command)     
            if (identifyPoints) {
                RcmdrTkmessageBox(title="Identify Points",
                    message=gettextRcmdr("Use left mouse button to identify points,\nright button to exit."),
                    icon="info", type="ok")
                doItAndPrint(paste("identify(rep(1, length(", var,
                    ")), ", var, ", rownames(", .activeDataSet,"))", sep=""))
                }           
            }
        else {
            command <- (paste("boxplot(", x, "~", .groups, ', ylab="', x, 
                '", xlab="', .groups,'"',
                ", data=", .activeDataSet, ")", sep=""))
            logger(command)
            justDoIt(command)
            if (identifyPoints) {
                RcmdrTkmessageBox(title="Identify Points",
                    message=gettextRcmdr("Use left mouse button to identify points,\nright button to exit."),
                    icon="info", type="ok")
                doItAndPrint(paste("identify(", .activeDataSet, "$", .groups, ", ", var,
                    ", rownames(", .activeDataSet,"))", sep=""))
                }
            }
        activateMenus()
        tkfocus(CommanderWindow())
        }
    groupsBox(boxPlot)
    OKCancelHelp(helpSubject="boxplot")
    tkgrid(getFrame(xBox), sticky="nw")    
    tkgrid(tklabel(identifyFrame, text=gettextRcmdr("Identify outliers with mouse"), justify="left"), 
        identifyCheckBox, sticky="w")
    tkgrid(identifyFrame, stick="w")
    tkgrid(groupsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

scatterPlot <- function(){
    require("car")
    initializeDialog(title=gettextRcmdr("Scatterplot"))
    .numeric <- Numeric()
    xBox <- variableListBox(top, .numeric, title=gettextRcmdr("x-variable (pick one)"))
    yBox <- variableListBox(top, .numeric, title=gettextRcmdr("y-variable (pick one)"))
    checkBoxes(frame="optionsFrame", boxes=c("identify", "jitterX", "jitterY", "boxplots", "lsLine", "smoothLine"),
        initialValues=c(0, 0, 0, 1, 1, 1), labels=gettextRcmdr(c("Identify points", "Jitter x-variable", "Jitter y-variable",
        "Marginal boxplots", "Least-squares line", "Smooth Line")))
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=5, orient="horizontal")
    subsetBox()
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        closeDialog()
        if (length(x) == 0 || length(y) == 0){
            errorCondition(recall=scatterPlot, message=gettextRcmdr("You must select two variables"))
            return()
            }
        if (x == y) {
            errorCondition(recall=scatterPlot, message=gettextRcmdr("x and y variables must be different"))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        jitter <- if ("1" == tclvalue(jitterXVariable) && "1" == tclvalue(jitterYVariable)) ", jitter=list(x=1, y=1)"
            else if ("1" == tclvalue(jitterXVariable)) ", jitter=list(x=1)"
            else if ("1" == tclvalue(jitterYVariable)) ", jitter=list(y=1)"
            else ""
        if("1" == tclvalue(identifyVariable)){
            RcmdrTkmessageBox(title="Identify Points",
                message=gettextRcmdr("Use left mouse button to identify points,\nright button to exit."),
                icon="info", type="ok")
            labels <- paste("rownames(", .activeDataSet, ")", sep="")
            }
        else labels <- "FALSE"
        box <- if ("1" == tclvalue(boxplotsVariable)) "'xy'" else "FALSE"
        line <- if("1" == tclvalue(lsLineVariable)) "lm" else "FALSE"
        smooth <- as.character("1" == tclvalue(smoothLineVariable))
        span <- as.numeric(tclvalue(sliderValue))
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" 
            else paste(", subset=", subset, sep="")
        tkdestroy(top)
        if (.groups == FALSE) {
            doItAndPrint(paste("scatterplot(", y, "~", x,
                ", reg.line=", line, ", smooth=", smooth, ", labels=", labels,
                ", boxplots=", box, ", span =", span/100, jitter,
                ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else {
            doItAndPrint(paste("scatterplot(", y, "~", x," | ", .groups,
                ", reg.line=", line, ", smooth=", smooth, ", labels=", labels,
                ", boxplots=", box, ", span=", span/100, jitter,
                ", by.groups=", .linesByGroup,
                ", data=", .activeDataSet, subset, ")", sep=""))
            }
        activateMenus()
        tkfocus(CommanderWindow())
        }
    groupsBox(scatterPlot, plotLinesByGroup=TRUE)
    OKCancelHelp(helpSubject="scatterplot")
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")    
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Span for smooth")), slider, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=5, columns=2)
    }

scatterPlotMatrix <- function(){
    require("car")
    initializeDialog(title=gettextRcmdr("Scatterplot Matrix"))
    variablesBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Select variables (three or more)"),
        selectmode="multiple", initialSelection=NULL)
    checkBoxes(frame="optionsFrame", boxes=c("lsLine", "smoothLine"), initialValues=rep(1,2),
        labels=gettextRcmdr(c("Least-squares lines", "Smooth lines")))
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=5, orient="horizontal")
    radioButtons(name="diagonal", buttons=c("density", "histogram", "boxplot", "oned", "qqplot", "none"),
        labels=gettextRcmdr(c("Density plots", "Histograms", "Boxplots", "One-dimensional scatterplots", "Normal QQ plots", "Nothing (empty)")),
        title=gettextRcmdr("On Diagonal"))
    subsetBox()
    onOK <- function(){
        variables <- getSelection(variablesBox)
        closeDialog()
        if (length(variables) < 3) {
            errorCondition(recall=scatterPlotMatrix, message=gettextRcmdr("Fewer than 3 variable selected."))
            return()
            }
        line <- if("1" == tclvalue(lsLineVariable)) "lm" else "FALSE"
        smooth <- as.character("1" == tclvalue(smoothLineVariable))
        span <- as.numeric(tclvalue(sliderValue))
        diag <- as.character(tclvalue(diagonalVariable))
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" 
            else paste(", subset=", subset, sep="")
        .activeDataSet <- ActiveDataSet()
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
        activateMenus()
        tkfocus(CommanderWindow())
        }
    groupsBox(scatterPlot, plotLinesByGroup=TRUE)
    OKCancelHelp(helpSubject="scatterplot.matrix")
    tkgrid(getFrame(variablesBox), sticky="nw")    
    tkgrid(optionsFrame, sticky="w")
    tkgrid(diagonalFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=6, columns=2)
    }

barGraph <- function(){
    initializeDialog(title=gettextRcmdr("Bar Graph"))
    variableBox <- variableListBox(top, Factors(), title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        variable <- getSelection(variableBox)
        closeDialog()
        if (length(variable) == 0){
            errorCondition(recall=barGraph, message=gettextRcmdr("You must select a variable"))
            return()
            }
        command <- paste("barplot(table(", ActiveDataSet(), "$", variable, '), xlab="',
            variable, '", ylab="Frequency")', sep="")
        logger(command)
        justDoIt(command)
        activateMenus()
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="barplot")
    tkgrid(getFrame(variableBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }

pieChart <- function(){
    initializeDialog(title=gettextRcmdr("Pie Chart"))
    variableBox <- variableListBox(top, Factors(), title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        variable <- getSelection(variableBox)
        closeDialog()
        if (length(variable) == 0){
            errorCondition(recall=pieChart, message=gettextRcmdr("You must select a variable"))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        command <- (paste("pie(table(", .activeDataSet, "$", variable, "), labels=levels(",
            .activeDataSet, "$", variable, '), main="', variable, '", col=rainbow(length(levels(',
            .activeDataSet, "$", variable, "))))", sep=""))
        logger(command)
        justDoIt(command)
        activateMenus()
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pie")
    tkgrid(getFrame(variableBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1)
    }

linePlot <- function(){
    initializeDialog(title=gettextRcmdr("Line Plot"))
    variablesFrame <- tkframe(top)
    .numeric <- Numeric()
    xBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("x variable (pick one)"))
    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("y variables (pick one or more)"), 
        selectmode="multiple", initialSelection=NULL)
    axisLabelVariable <- tclVar(gettextRcmdr("<use y-variable names>"))
    axisLabelFrame <- tkframe(top)
    axisLabelEntry <- tkentry(axisLabelFrame, width="40", textvariable=axisLabelVariable)
    axisLabelScroll <- tkscrollbar(axisLabelFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(axisLabelEntry, ...))
    tkconfigure(axisLabelEntry, xscrollcommand=function(...) tkset(axisLabelScroll, ...))
    legendFrame <- tkframe(top)
    legendVariable <- tclVar("0")
    legendCheckBox <- tkcheckbutton(legendFrame, variable=legendVariable)
    onOK <- function(){
        y <- getSelection(yBox)
        x <- getSelection(xBox)
        closeDialog()
        if (0 == length(x)) {
            errorCondition(recall=linePlot, message=gettextRcmdr("No x variable selected.")) 
            return()
            }
        if (0 == length(y)) {
            errorCondition(recall=linePlot, message=gettextRcmdr("No y variables selected.")) 
            return()
            }
        if (is.element(x, y)) {
            errorCondition(recall=linePlot, message=gettextRcmdr("x and y variables must be different."))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        .x <- na.omit(eval(parse(text=paste(.activeDataSet, "$", x, sep="")), envir=.GlobalEnv))
        if (!identical(order(.x), seq(along=.x))){
            response <- tclvalue(RcmdrTkmessageBox(message=gettextRcmdr("x-values are not in order.\nContinue?"), 
                icon="warning", type="okcancel", default="cancel"))
            if (response == "cancel") {
                onCancel()
                return()
                }
            }
        axisLabel <- tclvalue(axisLabelVariable)
        legend <- tclvalue(legendVariable) == "1"
        if (axisLabel == gettextRcmdr("<use y-variable names>")){
            axisLabel <- if (legend) ""
                else if(length(y) == 1) y
                else paste(paste("(", 1:length(y), ") ", y, sep=""), collapse=", ")
            }
        pch <- if (length(y) == 1) ", pch=1" else ""
        if (legend && length(y) > 1){
            mar <- par("mar")
            top <- 3.5 + length(y)
            command <- paste(".mar <- par(mar=c(", mar[1], ",", mar[2], ",", top, ",", mar[4], "))", sep="")
            logger(command)
            justDoIt(command)
            }
        command <- paste("matplot(", .activeDataSet, "$", x, ", ", .activeDataSet, "[, ",
            paste("c(", paste(paste('"', y, '"', sep=""), collapse=","), ")", sep=""),
            '], type="b", lty=1, ylab="', axisLabel, '"', pch, ")", sep="")
        logger(command)
        justDoIt(command)
        if (legend && length(y) > 1){
            n <- length(y)
            cols <- rep(1:6, 1 + n %/% 6)[1:n]
            logger(".xpd <- par(xpd=TRUE)")
            justDoIt(".xpd <- par(xpd=TRUE)")
            usr <- par("usr")
            command <- paste("legend(", usr[1], ", ", usr[4] + 1.2*top*strheight("x"), ", legend=", 
                paste("c(", paste(paste('"', y, '"', sep=""), collapse=","), ")", sep=""),
                ", col=c(", paste(cols, collapse=","), "), lty=1, pch=c(",
                paste(paste('"', as.character(1:n), '"', sep=""), collapse=","), "))", sep="")
            logger(command)
            justDoIt(command)
            logger("par(mar=.mar)")
            justDoIt("par(mar=.mar)")
            logger("par(xpd=.xpd)")
            justDoIt("par(xpd=.xpd)")
            }
        activateMenus()
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="matplot")
    tkgrid(getFrame(xBox), tklabel(variablesFrame, text="    "), getFrame(yBox), sticky="nw")
    tkgrid(variablesFrame, sticky="nw")    
    tkgrid(tklabel(axisLabelFrame, text=gettextRcmdr("Label for y-axis"), fg="blue"), sticky="w")
    tkgrid(axisLabelEntry, sticky="w")
    tkgrid(axisLabelScroll, sticky="ew")
    tkgrid(axisLabelFrame, sticky="w")
    tkgrid(tklabel(legendFrame, text=gettextRcmdr("Plot legend")),
        legendCheckBox, sticky="w")
    tkgrid(legendFrame, sticky="w")
    tkgrid(buttonsFrame, stick="w")
    dialogSuffix(rows=4, columns=1)
    }
    
QQPlot <- function()
# this function modified by Martin Maechler
{
    require("car")
    initializeDialog(title=gettextRcmdr("Quantile-Comparison (QQ) Plot"))
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(xBox)
        closeDialog()
       if (0 == length(x)) {
            errorCondition(recall=QQPlot, message=gettextRcmdr("You must select a variable.")) 
            return()
            }
        dist <- tclvalue(distVariable)
        save <- options(warn=-1)
        on.exit(options=save)
        retryMe <- function(msg) {
            Message(message= msg, type="error")
            QQPlot()
        }
        switch(dist,
               "norm" = { args <- 'dist= "norm"' },
               "t" =  {
                   df <- tclvalue(tDfVariable)
                   df.num <- as.numeric(df)
                   if (is.na(df.num) || df.num < 1) {
                       retryMe(gettextRcmdr("df for t must be a positive number."))
                       return()
                   }
                   args <- paste('dist="t", df=', df, sep="")
               },
               "chisq" = {
                   df <- tclvalue(chisqDfVariable)
                   df.num <- as.numeric(df)
                   if (is.na(df.num) || df.num < 1) {
                       retryMe(gettextRcmdr("df for chi-square must be a positive number."))
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
                       retryMe(gettextRcmdr("numerator and denominator \ndf for F must be positive numbers."))
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
        .activeDataSet <- ActiveDataSet()
        if ("1" == tclvalue(identifyVariable)){
            RcmdrTkmessageBox(title="Identify Points",
                message=gettextRcmdr("Use left mouse button to identify points,\nright button to exit."),
                icon="info", type="ok")
            labels <- paste("rownames(", .activeDataSet, ")", sep="")
            }
        else labels <- "FALSE"
        command <- paste("qq.plot", "(", .activeDataSet, "$", x, ", ", args,
                          ", labels=", labels, ")", sep="")
        doItAndPrint(command)
        activateMenus()
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="qq.plot")
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
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(tklabel(identifyFrame, text=gettextRcmdr("Identify observations with mouse")),
           identifyCheckBox, sticky="w")
    tkgrid(identifyFrame, sticky="w")
    tkgrid(tklabel(distFrame, text=gettextRcmdr("Distribution"), fg="blue"), columnspan=6, sticky="w")
    tkgrid(tklabel(distFrame, text=gettextRcmdr("Normal")), normalButton, sticky="w")
    tkgrid(tklabel(tDfFrame, text=gettextRcmdr("df = ")), tDfField, sticky="w")
    tkgrid(tklabel(distFrame, text="t"), tButton, tDfFrame, sticky="w")
    tkgrid(tklabel(chisqDfFrame, text=gettextRcmdr("df = ")), chisqDfField, sticky="w")
    tkgrid(tklabel(distFrame, text=gettextRcmdr("Chi-square")), chisqButton,
           chisqDfFrame, sticky="w")
    tkgrid(tklabel(FDfFrame, text=gettextRcmdr("Numerator df = ")), FDf1Field,
           tklabel(FDfFrame, text=gettextRcmdr("Denominator df = ")), FDf2Field, sticky="w")
    tkgrid(tklabel(distFrame, text="F"), FButton, FDfFrame, sticky="w")
    tkgrid(tklabel(otherParamsFrame, text=gettextRcmdr("Specify: ")),
           otherNameField, tklabel(otherParamsFrame, text=gettextRcmdr("Parameters: ")),
           otherParamsField, sticky="w")
    tkgrid(tklabel(distFrame, text=gettextRcmdr("Other")), otherButton,
           otherParamsFrame, sticky="w")
    tkgrid(distFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=5, columns=1)
    }

PlotMeans <- function(){
    initializeDialog(title=gettextRcmdr("Plot Means"))
    groupBox <- variableListBox(top, Factors(), title=gettextRcmdr("Factors (pick one or two)"), selectmode="multiple")
    responseBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Response Variable (pick one)"))
    onOK <- function(){
        groups <- getSelection(groupBox)
        response <- getSelection(responseBox)
        closeDialog()
        if (0 == length(groups)) {
            errorCondition(recall=PlotMeans, message=gettextRcmdr("No factors selected."))
            return()
            }
        if (2 < length(groups)) {
            errorCondition(recall=PlotMeans, message=gettextRcmdr("More than two factors selected."))
            return()
            }
        if (0 == length(response)) {
            errorCondition(recall=PlotMeans, message=gettextRcmdr("No response variable selected."))
            return()
            }
        .activeDataSet <- ActiveDataSet()
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
        activateMenus()
        tkfocus(CommanderWindow())
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
    OKCancelHelp(helpSubject="plotMeans")
    tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Error Bars", fg="blue")), sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Standard errors")), seButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Standard deviations")), sdButton, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Confidence intervals")), confIntButton,
        tklabel(optionsFrame, text=gettextRcmdr("   Level of confidence:")), levelEntry, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("No error bars")), noneButton, sticky="w")
    tkgrid(optionsFrame, columnspan=2, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=3, columns=2)
    }

Scatter3D <- function(){
    use.rgl <- options("Rcmdr")[[1]]$use.rgl
    if (length(use.rgl) == 0 || use.rgl) {
        require(rgl)
        require(mgcv)
        }    
    initializeDialog(title=gettextRcmdr("3D Scatterplot"))
    variablesFrame <- tkframe(top)
    .numeric <- Numeric()
    xBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("Explanatory variables (pick two)"), selectmode="multiple",
        initialSelection=NULL)
    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("Response variable (pick one)"))
    surfacesFrame <- tkframe(top)
    identifyPoints <- tclVar("0") 
    identifyPointsCheckBox <- tkcheckbutton(surfacesFrame, variable=identifyPoints)
    axisScales <- tclVar("1")
    axisScalesCheckBox <- tkcheckbutton(surfacesFrame, variable=axisScales)
    gridLines <- tclVar("1")
    gridLinesCheckBox <- tkcheckbutton(surfacesFrame, variable=gridLines)
    squaredResiduals <- tclVar("0")
    squaredResidualsCheckBox <- tkcheckbutton(surfacesFrame, variable=squaredResiduals)
    linearLSSurface <- tclVar("1")
    linearLSCheckBox <- tkcheckbutton(surfacesFrame, variable=linearLSSurface)
    quadLSSurface <- tclVar("0")
    quadLSCheckBox <- tkcheckbutton(surfacesFrame, variable=quadLSSurface)
    nonparSurface <- tclVar("0")
    nonparCheckBox <- tkcheckbutton(surfacesFrame, variable=nonparSurface)
    dfNonparVariable <- tclVar(gettextRcmdr("<auto>"))
    dfNonparField <- tkentry(surfacesFrame, width="6", textvariable=dfNonparVariable)
    additiveSurface <- tclVar("0")
    additiveCheckBox <- tkcheckbutton(surfacesFrame, variable=additiveSurface)
    dfAddVariable <- tclVar(gettextRcmdr("<auto>"))
    dfAddField <- tkentry(surfacesFrame, width="6", textvariable=dfAddVariable)
    ellipsoid <- tclVar("0")
    ellipsoidCheckBox <- tkcheckbutton(surfacesFrame, variable=ellipsoid)
    bgFrame <- tkframe(top)
    bgVariable <-tclVar("white")
    whiteButton <- tkradiobutton(bgFrame, variable=bgVariable, value="white")
    blackButton <- tkradiobutton(bgFrame, variable=bgVariable, value="black")
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        closeDialog()
        if (length(y) == 0) {
            errorCondition(recall=Scatter3D, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        if (2 != length(x)) {
            errorCondition(recall=Scatter3D, message=gettextRcmdr("You must select 2 explanatory variables."))
            return()
            }
        if (is.element(y, x)) {
            errorCondition(recall=Scatter3D, message=gettextRcmdr("Response and explanatory variables must be different."))
            return()
            }
        scales <- if (tclvalue(axisScales) == 1) "TRUE" else "FALSE"
        grid <- if (tclvalue(gridLines) == 1) "TRUE" else "FALSE"
        resids <- if(tclvalue(squaredResiduals) == 1) ', residuals="squares"' else ", residuals=TRUE"
        lin <- if(tclvalue(linearLSSurface) == 1) '"linear"'
        quad <- if(tclvalue(quadLSSurface) == 1) '"quadratic"'
        nonpar <- if (tclvalue(nonparSurface) == 1) '"smooth"'
        additive <- if (tclvalue(additiveSurface) == 1) '"additive"'
        surfaces <- c(lin, quad, nonpar, additive)
        nsurfaces <- length(surfaces)
        if (nsurfaces > 1) resids <- ""
        ellips <- if(tclvalue(ellipsoid) == 1) "TRUE" else "FALSE"
        opts <- options(warn=-1)
        dfNonpar <- tclvalue(dfNonparVariable)
        dfNonpar <- if (dfNonpar == gettextRcmdr("<auto>")) "" else paste(", df.smooth=", as.numeric(dfNonpar), sep="")
        dfAdd <- tclvalue(dfAddVariable)
        dfAdd <- if (dfAdd == gettextRcmdr("<auto>")) "" else paste(", df.additive=", as.numeric(dfAdd), sep="")
        options(opts)
        fit <- if (nsurfaces == 0) ", surface=FALSE"
            else if (nsurfaces == 1) paste(", fit=", surfaces, sep="")
            else paste(", fit=c(", paste(surfaces, collapse=","), ")", sep="")
        bg <- tclvalue(bgVariable)
        .activeDataSet <- ActiveDataSet()
        if (.groups != FALSE){ 
            groups <- paste(", groups=", .activeDataSet, "$", .groups, sep="")
            parallel <- paste(", parallel=", .linesByGroup, sep="")
            }
        else groups <- parallel <- ""                   
        command <- paste("scatter3d(", .activeDataSet, "$", x[1], ", ", 
            .activeDataSet, "$", y, ", ", .activeDataSet, "$", x[2], fit, resids, dfNonpar, 
            dfAdd, groups, parallel, ', bg="', bg, '", axis.scales=', scales, ', grid=', grid,
            ', ellipsoid=', ellips, ', xlab="', x[1], '", ylab="', y, '", zlab="', x[2], '")', sep="")
        doItAndPrint(command)
        putRcmdr("rgl", TRUE)
        command <- paste("identify3d(", .activeDataSet, "$", x[1], ", ", 
            .activeDataSet, "$", y, ", ", .activeDataSet, "$", x[2], groups,
            ', axis.scales=', scales, 
            ", labels=row.names(", .activeDataSet, "))", sep="")
        putRcmdr("Identify3d", command)
        .Tcl("update")
        if (tclvalue(identifyPoints) == 1){ 
            RcmdrTkmessageBox(title="Identify Points",
                message=gettextRcmdr("Drag right mouse button to identify points,\nclick right button to exit."),
                icon="info", type="ok") 
            doItAndPrint(command)
            }
        activateMenus()
        tkfocus(CommanderWindow())
        rgl.bringtotop()
        }
    groupsBox(Scatter3D, plotLinesByGroup=TRUE, plotLinesByGroupsText=gettextRcmdr("Parallel regression surfaces"))
    OKCancelHelp(helpSubject="Scatter3DDialog")
    tkgrid(getFrame(yBox), tklabel(variablesFrame, text="  "), getFrame(xBox), sticky="nw")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(tklabel(surfacesFrame, text=gettextRcmdr("Identify observations\nwith mouse")), identifyPointsCheckBox, sticky="w")
    tkgrid(tklabel(surfacesFrame, text=gettextRcmdr("Show axis scales")), axisScalesCheckBox, sticky="w")
    tkgrid(tklabel(surfacesFrame, text=gettextRcmdr("Show surface grid lines")), gridLinesCheckBox, sticky="w")
    tkgrid(tklabel(surfacesFrame, text=gettextRcmdr("Show squared residuals")), squaredResidualsCheckBox, sticky="w")
    tkgrid(tklabel(surfacesFrame, text=gettextRcmdr("Surfaces to Fit"), fg="blue"), sticky="w")
    tkgrid(tklabel(surfacesFrame, text=gettextRcmdr("Linear least-squares")), linearLSCheckBox, sticky="w")
    tkgrid(tklabel(surfacesFrame, text=gettextRcmdr("Quadratic least-squares")), quadLSCheckBox, sticky="w")
    dfLabel <- tklabel(surfacesFrame, text=gettextRcmdr("df = "))
    tkgrid(tklabel(surfacesFrame, text=gettextRcmdr("Smooth regression")), nonparCheckBox, 
        dfLabel, dfNonparField, sticky="w")
    tkgrid.configure(dfLabel, sticky="e")
    tkgrid(tklabel(surfacesFrame, text=gettextRcmdr("Additive regression")), additiveCheckBox, 
        tklabel(surfacesFrame, text=gettextRcmdr("df(each term) = ")), dfAddField, sticky="w")
    tkgrid(tklabel(surfacesFrame, text=gettextRcmdr("Plot 50% concentration ellipsoid")), ellipsoidCheckBox, sticky="w")
    tkgrid(surfacesFrame, sticky="w") 
    tkgrid(tklabel(bgFrame, text=gettextRcmdr("Background Color"), fg="blue"), sticky="w", columnspan=2)
    tkgrid(tklabel(bgFrame, text=gettextRcmdr("Black")), blackButton, sticky="w")
    tkgrid(tklabel(bgFrame, text=gettextRcmdr("White")), whiteButton, sticky="w")
    tkgrid(bgFrame, sticky="w")
    tkgrid(groupsFrame, sticky="w")
    tkgrid(buttonsFrame, stick="w")
    dialogSuffix(rows=5, columns=1)
    }

Identify3D <- function(){
    if (0 == rgl.cur()) {
        Message(message=gettextRcmdr("There is no current RGL graphics device."),
            type="error")
        return()
        }
    RcmdrTkmessageBox(title="Identify Points",
        message=gettextRcmdr("Drag right mouse button to identify points,\nclick right button to exit."),
        icon="info", type="ok") 
    command <- getRcmdr("Identify3d")
    doItAndPrint(command)
    }
    
saveBitmap <- function(){
    if (1 == dev.cur()) {
        Message(gettextRcmdr("There is no current graphics device to save."), type="error")
        return()
        }
    initializeDialog(title=gettextRcmdr("Save Graph as Bitmap"))
    radioButtons(name="filetype", buttons=c("png", "jpeg"), labels=c("PNG", "JPEG"), title=gettextRcmdr("Graphics File Type"))
    sliderFrame <- tkframe(top)
    widthVariable <- tclVar("500")
    widthSlider <- tkscale(sliderFrame, from=200, to=1000, showvalue=TRUE, variable=widthVariable,
        resolution=25, orient="horizontal")
    heightVariable <- tclVar("500")
    heightSlider <- tkscale(sliderFrame, from=200, to=1000, showvalue=TRUE, variable=heightVariable,
        resolution=25, orient="horizontal")
    onOK <- function(){
        closeDialog()
        width <- tclvalue(widthVariable)
        height <- tclvalue(heightVariable)
        type <- tclvalue(filetypeVariable)
        if (type == "png"){
            ext <- "png"
            filetypes <- gettextRcmdr('{"PNG Files" {".png" ".PNG"}} {"All Files" {"*"}}')
            initial <- "RGraph.png"
            }
        else{
            ext <- "jpg"
            filetypes <- gettextRcmdr('{"JPEG Files" {".jpg" ".JPG" ".jpeg" ".JPEG"}} {"All Files" {"*"}}')
            initial <- "RGraph.jpg"
            }
        filename <- tclvalue(tkgetSaveFile(filetypes=filetypes, defaultextension=ext, initialfile=initial))
        if (filename == "") return()
        command <- paste('dev.print(', type, ', filename="', filename, '", width=', width, ', height=', height, ')', sep="")
        doItAndPrint(command)
        Message(paste(gettextRcmdr("Graph saved to file"), filename), type="note")
        }
    OKCancelHelp(helpSubject="png")
    tkgrid(filetypeFrame, sticky="w")
    tkgrid(tklabel(sliderFrame, text=gettextRcmdr("Width (pixels)")), widthSlider, sticky="sw")
    tkgrid(tklabel(sliderFrame, text=gettextRcmdr("Height (pixels)")), heightSlider, sticky="sw")
    tkgrid(sliderFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1)
    }

savePDF <- function(){
    if (1 == dev.cur()) {
        Message(gettextRcmdr("There is no current graphics device to save."), type="error")
        return()
        }
    initializeDialog(title=gettextRcmdr("Save Graph as PDF/Postscript"))
    radioButtons(name="filetype", buttons=c("pdf", "postscript", "eps"), 
        labels=gettextRcmdr(c("PDF", "Postscript", "Encapsulated Postscript")), title=gettextRcmdr("Graphics File Type"))
    sliderFrame <- tkframe(top)
    widthVariable <- tclVar("5")
    widthSlider <- tkscale(sliderFrame, from=3, to=10, showvalue=TRUE, variable=widthVariable,
        resolution=0.1, orient="horizontal")
    heightVariable <- tclVar("5")
    heightSlider <- tkscale(sliderFrame, from=3, to=10, showvalue=TRUE, variable=heightVariable,
        resolution=0.1, orient="horizontal")
    pointSizeVariable <- tclVar("10")
    pointSizeSlider <- tkscale(sliderFrame, from=6, to=14, showvalue=TRUE, variable=pointSizeVariable,
        resolution=1, orient="horizontal")
    onOK <- function(){
        closeDialog()
        width <- tclvalue(widthVariable)
        height <- tclvalue(heightVariable)
        type <- tclvalue(filetypeVariable)
        pointsize <- tclvalue(pointSizeVariable)
        if (type == "pdf"){
            ext <- "pdf"
            filetypes <- gettextRcmdr('{"PDF Files" {".pdf" ".PDF"}} {"All Files" {"*"}}')
            initial <- "RGraph.pdf"
            }
        else if (type == "postscript") {
            ext <- "ps"
            filetypes <- gettextRcmdr('{"Postscript Files" {".ps" ".PS"}} {"All Files" {"*"}}')
            initial <- "RGraph.ps"
            }
        else {
            ext <- "eps"
            filetypes <- gettextRcmdr('{"Encapsulated Postscript Files" {".eps" ".EPS"}} {"All Files" {"*"}}')
            initial <- "RGraph.eps"
            }
        filename <- tclvalue(tkgetSaveFile(filetypes=filetypes, defaultextension=ext, initialfile=initial))
        if (filename == "") return()
        command <- if (type == "eps") paste('dev.copy2eps(file="', filename, '", width=', width, ', height=', height, 
                ', pointsize=', pointsize, ')', sep="")
            else paste('dev.print(', type, ', file="', filename, '", width=', width, ', height=', height, 
                ', pointsize=', pointsize, ')', sep="")
        doItAndPrint(command)
        Message(paste(gettextRcmdr("Graph saved to file"), filename), type="note")
        }
    OKCancelHelp(helpSubject="pdf")
    tkgrid(filetypeFrame, sticky="w")
    tkgrid(tklabel(sliderFrame, text=gettextRcmdr("Width (inches)")), widthSlider, sticky="sw")
    tkgrid(tklabel(sliderFrame, text=gettextRcmdr("Height (inches)")), heightSlider, sticky="sw")
    tkgrid(tklabel(sliderFrame, text=gettextRcmdr("Text size (points)")), pointSizeSlider, sticky="sw")
    tkgrid(sliderFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1)
    }

saveRglGraph <- function(){
    if (0 == rgl.cur()) {
        Message(message=gettextRcmdr("There is no current RGL graphics device to save."),
            type="error")
        return()
        }  
    ext <- "png"
    filetypes <- gettextRcmdr('{"PNG Bitmap Files" {".png" ".PNG"}} {"All Files" {"*"}}')
    initial <- "RGLGraph.png"  
    filename <- tclvalue(tkgetSaveFile(filetypes=filetypes, defaultextension=ext, initialfile=initial))
    if (filename == "") return()
    command <- paste('rgl.snapshot("', filename, '")', sep="")
    doItAndPrint(command)
    Message(paste(gettextRcmdr("Graph saved to file"), filename), type="note")
    }
    
# The following function by Richard Heiberger, with small modifications by J. Fox

Xyplot <- function() {
    require("lattice")
    initializeDialog(title=gettextRcmdr("XY Conditioning Plot"))
    predictorBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Explanatory variables (pick one or more)"), selectmode="multiple")
    responseBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Response variables (pick one or more)"), selectmode="multiple")
    groupsBox <- variableListBox(top, Factors(), title=gettextRcmdr("Groups (pick zero or more)"), selectmode="multiple", initialSelection=FALSE)
    checkBoxes(frame="optionsFrame",
               boxes=c("auto.key", "outer"),
               initialValues=c(1,0),
               labels=gettextRcmdr(c("Automatically draw key", 
                "Different panels for different responses")))
    radioButtons(name="x.relation",
                 buttons=c("same", "free", "sliced"),
                 labels=gettextRcmdr(c("Identical", "Free", "Same range")),
                 title=gettextRcmdr("X-Axis Scales in Different Panels"))
    radioButtons(name="y.relation",
                 buttons=c("same", "free", "sliced"),
                 labels=gettextRcmdr(c("Identical", "Free", "Same range")),
                 title=gettextRcmdr("Y-Axis Scales in Different Panels"))
    onOK <- function() {
        predictor <- getSelection(predictorBox)
        response <- getSelection(responseBox)
        groups <- getSelection(groupsBox)
        closeDialog()
        if (0 == length(response)) {
            errorCondition(recall=Xyplot, message=gettextRcmdr("At least one response variable must be selected."))
            return()
            }
        if (0 == length(predictor)) {
            errorCondition(recall=Xyplot, message=gettextRcmdr("At least one explanatory variable must be selected."))
            return()
            }
        auto.key <- ("1" == tclvalue(auto.keyVariable))
        outer    <- ("1" == tclvalue(outerVariable))
        x.relation <- as.character(tclvalue(x.relationVariable))
        y.relation <- as.character(tclvalue(y.relationVariable))
        .activeDataSet <- ActiveDataSet()
        xyplot.command <- paste("xyplot(",
                                paste(response, collapse=' + '),
                                " ~ ",
                                paste(predictor, collapse=' + '),
                                if (length(groups) > 0)
                                   paste(" | ",
                                         paste(groups, collapse=' + ')
                                         ) else "",
                                if (outer) ", outer=TRUE",
                                if (length(groups)==0) {
                                  if (outer) paste(", layout=c(",
                                                   length(predictor),
                                                   ",",
                                                   length(response),
                                                   ")")
                                }
                                else {  ## (length(groups)>0)
                                  if (outer) {
                                    group.levels <- prod(sapply(groups, d.f=get(.activeDataSet),
                                                                function(g, d.f) length(levels(d.f[[g]]))))
                                    paste(", layout=c(",
                                          group.levels,
                                          "*",
                                          length(predictor),
                                          ",",
                                          length(response),
                                          ")",
                                          ## ", between=list(x=c(0,0, 1, 0,0), y=1)",
                                          ", between=list(x=c(",
                                          paste(rep(c(rep(0, group.levels-1), 1),
                                                    length=group.levels*length(predictor)-1),
                                                collapse=","),
                                          "), y=1)")
                                  }
                                },
                                if (auto.key) ", auto.key=TRUE" else "",
                                paste(", scales=list(x=list(relation='",
                                      x.relation,
                                      "'), y=list(relation='",
                                      y.relation,
                                      "'))", sep=""),
                                ", data=", .activeDataSet, ')', sep="")
        doItAndPrint(xyplot.command)
        activateMenus()
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="xyplot")
    tkgrid(getFrame(predictorBox), getFrame(responseBox), sticky="nw")
    tkgrid(getFrame(groupsBox), sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(x.relationFrame, sticky="w")
    tkgrid(y.relationFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=6, columns=2)
    }

