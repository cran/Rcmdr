# last modified 28 May 2004 by J. Fox

# File menu dialogs

loadLog <- function(){
    logFile <- tclvalue(tkgetOpenFile(filetypes='{"Script Files" {".R"}} {"All Files" {"*"}}',
        defaultextension="log"))
    if (logFile == "") return()
    fileCon <- file(logFile, "r")
    contents <- readLines(fileCon)
    close(fileCon)
    assign(".logFileName", logFile, envir=.GlobalEnv)
    if (tclvalue(tkget(.log, "1.0", "end")) != "\n"){
        response2 <- tkmessageBox(message="Save current log file?",
                icon="question", type="yesno", default="yes")
        if ("yes" == tclvalue(response2)) saveLog()
        }
    tkdelete(.log, "1.0", "end")
    tkinsert(.log, "end", paste(contents, collapse="\n"))
    }
    
saveLog <- function() {
    if (is.null(.logFileName)) {
        saveLogAs()
        return()
        }
    log <- tclvalue(tkget(.log, "1.0", "end"))
    fileCon <- file(.logFileName, "w")
    cat(log, file = fileCon)
    close(fileCon)
    }

saveLogAs <- function() {
    logFile <- tclvalue(tkgetSaveFile(filetypes='{"Script Files" {".R"}} {"All Files" {"*"}}',
        defaultextension="R", initialfile="RCommander.R"))
    log <- tclvalue(tkget(.log, "1.0", "end"))
    fileCon <- file(logFile, "w")
    cat(log, file = fileCon)
    close(fileCon)
    assign(".logFileName", logFile, envir=.GlobalEnv)
    }

saveOutput <- function() {
    if (is.null(.outputFileName)) {
        saveOutputAs()
        return()
        }
    output <- tclvalue(tkget(.output, "1.0", "end"))
    fileCon <- file(.outputFileName, "w")
    cat(output, file = fileCon)
    close(fileCon)
    }

saveOutputAs <- function() {
    outputFile <- tclvalue(tkgetSaveFile(filetypes='{"Output Files" {".txt"}} {"All Files" {"*"}}',
        defaultextension="txt", initialfile="RCommander.txt"))
    output <- tclvalue(tkget(.output, "1.0", "end"))
    fileCon <- file(outputFile, "w")
    cat(output, file = fileCon)
    close(fileCon)
    assign(".outputFileName", outputFile, envir=.GlobalEnv)
    }
    
closeCommander <- function(){
    globals <- c(".activeDataSet", ".activeModel", ".attach.data.set", ".command.text.color", ".commander", 
        ".console.output", ".dataSetLabel", ".dataSetName", ".double.click", ".factors",
        ".grab.focus", ".log", ".log.commands", ".logFileName", ".logFont", ".log.text.color",
        ".modelLabel", ".modelName", ".modelNumber", ".modelWithSubset", 
        ".numeric", "oldPager", ".operatorFont", ".output.text.color", ".outputFileName", 
        ".rgl", ".saveOptions", ".sort.names",
        ".twoLevelFactors", ".variables")
    response <- tclvalue(tkmessageBox(message="Exit?",
        icon="question", type="okcancel", default="cancel"))
    if (response == "cancel") return(invisible(response))
    if (.rgl) rgl.quit()
    if (!is.null(.activeDataSet) && .attach.data.set) 
        justDoIt(logger(paste("detach(", .activeDataSet, ")", sep="")))
    assign(".activeDataSet", NULL, envir=.GlobalEnv)
    assign(".activeModel", NULL, envir=.GlobalEnv)
    if (.log.commands && tclvalue(tkget(.log, "1.0", "end")) != "\n"){
        response2 <- tkmessageBox(message="Save script file?",
                icon="question", type="yesno", default="yes")
        if ("yes" == tclvalue(response2)) saveLog()
        }
    if (!.console.output && tclvalue(tkget(.output, "1.0", "end")) != "\n"){
        response3 <- tkmessageBox(message="Save ouptut file?",
                icon="question", type="yesno", default="yes")
        if ("yes" == tclvalue(response3)) saveOutput()
        }
    if (.Platform$OS.type != "windows") options(.oldPager)
    options(.saveOptions)
    tkdestroy(.commander)
    tkwait <- options("Rcmdr")[[1]]$tkwait  # to address problem in Debian Linux
    if ((!is.null(tkwait)) && tkwait) tclvalue(.commander.done) <<- "1"   
    which.globals <- sapply(globals, exists, envir=.GlobalEnv)
    remove(list=globals[which.globals], envir=.GlobalEnv)
    return(invisible(response))
    }
    
closeCommanderAndR <- function(){
    response <- closeCommander()
    if (response == "cancel") return()
    quit(save="no")
    }

Options <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Commander Options")
    current <- options("Rcmdr")[[1]]
    console.output <- if (is.null(current$console.output)) FALSE else current$console.output
    log.commands <- if (is.null(current$log.commands)) TRUE else current$log.commands
    log.font.size <- if (is.null(current$log.font.size)) 10 else current$log.font.size
    log.width <- if (is.null(current$log.width)) 80 else current$log.width
    log.height <- if (!is.null(current$log.height)) current$log.height
                    else if (!log.commands) 0 else 10
    output.height <- if (!is.null(current$output.height)) current$output.height
        else if (console.output) 0 else 2*log.height 
    contrasts <- if (is.null(current$contrasts)) c("contr.Treatment", "contr.poly") else current$contrasts
    grab.focus <- if (is.null(current$grab.focus)) TRUE else current$grab.focus
    double.click <- if (is.null(current$double.click)) FALSE else current$double.click
    sort.names <- if (is.null(current$sort.names)) TRUE else current$sort.names
    show.edit.button <- if (is.null(current$show.edit.button)) TRUE else current$show.edit.button
    scale.factor <- current$scale.factor
    default.font.size <- if (is.null(current$default.font.size)) 10 else current$default.font.size
    default.font <- if(is.null(current$default.font)) paste("*helvetica-medium-r-normal-*-",
            default.font.size, "*", sep="") else current$default.font
    consoleOutputVar <- tclVar(console.output)
    consoleOutputCheckBox <- tkcheckbutton(top, variable=consoleOutputVar)
    logCommandsVar <- tclVar(log.commands)
    logCommandsCheckBox <- tkcheckbutton(top, variable=logCommandsVar)
    logFontSizeVar <- tclVar(log.font.size)
    logFontSizeSlider <- tkscale(top, from=6, to=20, showvalue=TRUE, variable=logFontSizeVar,
        resolution=1, orient="horizontal")
    logWidthVar <- tclVar(log.width)
    logWidthSlider <- tkscale(top, from=30, to=120, showvalue=TRUE, variable=logWidthVar,
        resolution=5, orient="horizontal")    
    logHeightVar <- tclVar(log.height)
    logHeightSlider <- tkscale(top, from=0, to=25, showvalue=TRUE, variable=logHeightVar,
        resolution=1, orient="horizontal")   
    outputHeightVar <- tclVar(output.height)
    outputHeightSlider <- tkscale(top, from=0, to=50, showvalue=TRUE, variable=outputHeightVar,
        resolution=5, orient="horizontal")   
    contrasts1 <- tclVar(contrasts[1])
    contrasts2 <- tclVar(contrasts[2])
    contrastsFrame <- tkframe(top)
    contrasts1Entry <- tkentry(contrastsFrame, width="15", textvariable=contrasts1)  
    contrasts2Entry <- tkentry(contrastsFrame, width="15", textvariable=contrasts2) 
    grabFocusVar <- tclVar(as.numeric(grab.focus))
    grabFocusCheckBox <- tkcheckbutton(top, variable=grabFocusVar)
    doubleClickVar <- tclVar(as.numeric(double.click))
    doubleClickCheckBox <- tkcheckbutton(top, variable=doubleClickVar)
    sortNamesVar <- tclVar(as.numeric(sort.names))
    sortNamesCheckBox <- tkcheckbutton(top, variable=sortNamesVar)
    showEditButtonVar <- tclVar(as.numeric(show.edit.button))
    showEditButtonCheckBox <- tkcheckbutton(top, variable=showEditButtonVar)
    scaleFactorVar <- tclVar(if (is.null(scale.factor)) 1.0 else scale.factor)
    scaleFactorSlider <- tkscale(top, from=0.2, to=3.0, showvalue=TRUE, variable=scaleFactorVar,
        resolution=0.2, orient="horizontal")        
    defaultFont <- tclVar(default.font)
    defaultFontEntry <- tkentry(top, width="30", textvariable=scaleFactorVar)                  
    onOK <- function(){
        log.font.size <- round(as.numeric(tclvalue(logFontSizeVar)))
        log.width <- round(as.numeric(tclvalue(logWidthVar)))
        log.height <- as.numeric(tclvalue(logHeightVar))
        log.commands <- as.logical(tclvalue(logCommandsVar) == "1") && (log.height != 0)
        output.height <- as.numeric(tclvalue(outputHeightVar))
        console.output <- as.logical(tclvalue(consoleOutputVar) == "1") || (output.height == 0)
        contrasts <- c(tclvalue(contrasts1), tclvalue(contrasts2))
        grab.focus <- tclvalue(grabFocusVar) == 1
        double.click <- tclvalue(doubleClickVar) == 1
        sort.names <- tclvalue(sortNamesVar) == 1
        show.edit.button <- tclvalue(showEditButtonVar) == 1
        scale.factor <- round(as.numeric(tclvalue(scaleFactorVar)), 1)
        if (scale.factor == 1) scale.factor <- NULL
        default.font <- tclvalue(defaultFont)
        options <- list(
            log.font.size=log.font.size,
            log.width=log.width,
            log.height=log.height,
            log.commands=log.commands,
            output.height=output.height,
            console.output=console.output,
            contrasts=contrasts,
            grab.focus=grab.focus,
            double.click=double.click,
            sort.names=sort.names,
            show.edit.button=show.edit.button
            )
        if (.Platform$OS.type == "windows") options$scale.factor <- scale.factor
        else options$default.font <- default.font
        options(Rcmdr=options)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)  
        closeCommander()
        Commander()
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="Restart Commander", fg="darkgreen", command=onOK, 
        default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(Commander)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Log commands to script window"), logCommandsCheckBox, sticky="e")
    tkgrid.configure(logCommandsCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Log-font size (points)"), logFontSizeSlider, sticky="se")
    tkgrid.configure(logFontSizeSlider, sticky="w")
    tkgrid(tklabel(top, text="Log width (characters)"), logWidthSlider, sticky="se")
    tkgrid.configure(logWidthSlider, sticky="w")
    tkgrid(tklabel(top, text="Log height (lines)"), logHeightSlider, sticky="se")
    tkgrid.configure(logHeightSlider, sticky="w")
    tkgrid(tklabel(top, text=" "), sticky="w")
    tkgrid(tklabel(top, text="Send output to R Console"), consoleOutputCheckBox, sticky="e")
    tkgrid.configure(consoleOutputCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Output height (lines)"), outputHeightSlider, sticky="se")
    tkgrid.configure(outputHeightSlider, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Unordered factors"), tklabel(contrastsFrame, text="   "),
        tklabel(contrastsFrame, text="Ordered factors"), sticky="w")
    tkgrid(contrasts1Entry, tklabel(contrastsFrame, text="   "), contrasts2Entry, sticky="w")
    tkgrid(tklabel(top, text="Contrasts"), contrastsFrame, sticky="se")
    tkgrid.configure(contrastsFrame, sticky="sw")
    tkgrid(tklabel(top, text="Active window grabs focus"), grabFocusCheckBox, sticky="e")
    tkgrid.configure(grabFocusCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Double-click presses OK button"), doubleClickCheckBox, sticky="e")
    tkgrid.configure(doubleClickCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Sort variable names alphabetically"), sortNamesCheckBox, sticky="e")
    tkgrid.configure(sortNamesCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Show edit button"), showEditButtonCheckBox, sticky="e")
    tkgrid.configure(showEditButtonCheckBox, sticky="w")
    if (.Platform$OS.type == "windows"){
        tkgrid(tklabel(top, text="Scale factor for Tk elements"), scaleFactorSlider, sticky="se")
        tkgrid.configure(scaleFactorSlider, sticky="w")
        }
    else {
        tkgrid(tklabel(top, text="Default font"), defaultFontEntry, sticky="e")
        tkgrid.configure(defaultFontEntry, sticky="w")
        }
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:10) tkgrid.rowconfigure(top, row, weight=0)
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

setOutputWidth <- function(){
    top <- tktoplevel()
    tkwm.title(top, "Reset Output Width")
    output.width <- unlist(options("width"))
    outputWidthVar <- tclVar(output.width)
    logWidthSlider <- tkscale(top, from=20, to=200, showvalue=TRUE, variable=outputWidthVar,
        resolution=10, orient="horizontal")    
    onOK <- function(){
        output.width <- round(as.numeric(tclvalue(outputWidthVar)))
        doItAndPrint(paste("options(width=", output.width, ")", sep=""))
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    onCancel <- function() {
        if (.grab.focus) tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }    
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", command=onOK, width=12,
        default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(options)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Output width (characters)"), logWidthSlider, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
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
   
