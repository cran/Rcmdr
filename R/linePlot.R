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