# last modified 29 July 2003 by J. Fox

# File menu dialogs

loadLog <- function(){
    logFile <- tclvalue(tkgetOpenFile(filetypes='{"Log Files" {".log"}} {"All Files" {"*"}}',
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
    logFile <- tclvalue(tkgetSaveFile(filetypes='{"Log Files" {".log"}} {"All Files" {"*"}}',
        defaultextension="log", initialfile="RCommander.log"))
    log <- tclvalue(tkget(.log, "1.0", "end"))
    fileCon <- file(logFile, "w")
    cat(log, file = fileCon)
    close(fileCon)
    assign(".logFileName", logFile, envir=.GlobalEnv)
    }


closeCommander <- function(){
    globals <- c(".activeDataSet", ".activeModel", ".attachDataSet", ".commander", 
        ".dataSetLabel", ".dataSetName", ".double.click", ".factors", ".groups", 
        ".linesByGroup", ".log", ".logCommands", ".logFileName", ".logFont", 
        ".grab.focus", ".modelLabel", ".modelName", ".modelNumber", ".modelWithSubset", 
        ".numeric", "oldPager", ".operatorFont", ".saveOptions", ".twoLevelFactors", 
        ".variables")
    response <- tclvalue(tkmessageBox(message="Exit?",
        icon="question", type="okcancel", default="cancel"))
    if (response == "cancel") return(invisible(response))
    if (!is.null(.activeDataSet) && (tclvalue(.attachDataSet) == "1")) 
        justDoIt(logger(paste("detach(", .activeDataSet, ")", sep="")))
    assign(".activeDataSet", NULL, envir=.GlobalEnv)
    assign(".activeModel", NULL, envir=.GlobalEnv)
    if (tclvalue(tkget(.log, "1.0", "end")) != "\n"){
        response2 <- tkmessageBox(message="Save log file?",
                icon="question", type="yesno", default="yes")
        if ("yes" == tclvalue(response2)) saveLog()
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
    log.font.size <- if (is.null(current$log.font.size)) 10 else current$log.font.size
    log.width <- if (is.null(current$log.width)) 70 else current$log.width
    log.height <- if (is.null(current$log.height)) 15 else current$log.height
    contrasts <- if (is.null(current$contrasts)) c("contr.Treatment", "contr.poly") else current$contrasts
    grab.focus <- if (is.null(current$grab.focus)) TRUE else current$grab.focus
    double.click <- if (is.null(current$double.click)) FALSE else current$double.click
    scale.factor <- current$scale.factor
    default.font.size <- if (is.null(current$default.font.size)) 10 else current$default.font.size
    default.font <- if(is.null(current$default.font)) paste("*helvetica-medium-r-normal-*-",
            default.font.size, "*", sep="") else current$default.font
    logFontSizeVar <- tclVar(log.font.size)
    logFontSizeSlider <- tkscale(top, from=6, to=20, showvalue=TRUE, variable=logFontSizeVar,
        resolution=1, orient="horizontal")
    logWidthVar <- tclVar(log.width)
    logWidthSlider <- tkscale(top, from=30, to=120, showvalue=TRUE, variable=logWidthVar,
        resolution=5, orient="horizontal")    
    logHeightVar <- tclVar(log.height)
    logHeightSlider <- tkscale(top, from=5, to=35, showvalue=TRUE, variable=logHeightVar,
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
    scaleFactorVar <- tclVar(if (is.null(scale.factor)) 1.0 else scale.factor)
    scaleFactorSlider <- tkscale(top, from=0.2, to=3.0, showvalue=TRUE, variable=scaleFactorVar,
        resolution=0.2, orient="horizontal")        
    defaultFont <- tclVar(default.font)
    defaultFontEntry <- tkentry(top, width="30", textvariable=scaleFactorVar)                  
    onOK <- function(){
        log.font.size <- round(as.numeric(tclvalue(logFontSizeVar)))
        log.width <- round(as.numeric(tclvalue(logWidthVar)))
        log.height <- as.numeric(tclvalue(logHeightVar))
        contrasts <- c(tclvalue(contrasts1), tclvalue(contrasts2))
        grab.focus <- tclvalue(grabFocusVar) == 1
        double.click <- tclvalue(doubleClickVar) == 1
        scale.factor <- round(as.numeric(tclvalue(scaleFactorVar)), 1)
        if (scale.factor == 1) scale.factor <- NULL
        default.font <- tclvalue(defaultFont)
        options <- list(
            log.font.size=log.font.size,
            log.width=log.width,
            log.height=log.height,
            contrasts=contrasts,
            grab.focus=grab.focus,
            double.click=double.click
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
    OKbutton <- tkbutton(buttonsFrame, text="Restart Commander", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") if (.grab.focus) tkgrab.release(top)
        help(Commander)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Log-font size (points)"), logFontSizeSlider, sticky="se")
    tkgrid.configure(logFontSizeSlider, sticky="w")
    tkgrid(tklabel(top, text="Log width (characters)"), logWidthSlider, sticky="se")
    tkgrid.configure(logWidthSlider, sticky="w")
    tkgrid(tklabel(top, text="Log height (lines)"), logHeightSlider, sticky="se")
    tkgrid.configure(logHeightSlider, sticky="w")
    tkgrid(tklabel(contrastsFrame, text="Unordered factors"), tklabel(contrastsFrame, text="   "),
        tklabel(contrastsFrame, text="Ordered factors"), sticky="w")
    tkgrid(contrasts1Entry, tklabel(contrastsFrame, text="   "), contrasts2Entry, sticky="w")
    tkgrid(tklabel(top, text="Contrasts"), contrastsFrame, sticky="se")
    tkgrid.configure(contrastsFrame, sticky="sw")
    tkgrid(tklabel(top, text="Active window grabs focus"), grabFocusCheckBox, sticky="e")
    tkgrid.configure(grabFocusCheckBox, sticky="w")
    tkgrid(tklabel(top, text="Double-click presses OK button"), doubleClickCheckBox, sticky="e")
    tkgrid.configure(doubleClickCheckBox, sticky="w")
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
    for (row in 0:7) tkgrid.rowconfigure(top, row, weight=0)
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
