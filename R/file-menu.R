# last modified 1 Sept 2004 by J. Fox

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

saveWorkspaceAs <- function(){
    saveFile <- tclvalue(tkgetSaveFile(filetypes='{"All Files" {"*"}}',
        defaultextension="", initialfile=".RData"))
    save(list=ls(envir=.GlobalEnv), file=saveFile)
    assign(".saveFileName", saveFile, envir=.GlobalEnv)
    }

saveWorkspace <- function() {
    if (is.null(.saveFileName)) saveWorkspaceAs()
    else save(list=ls(envir=.GlobalEnv), file=.saveFileName)
    }
    
closeCommander <- function(){
    globals <- c(".activeDataSet", ".activeModel", ".attach.data.set", ".command.text.color", ".commander", ".grab.focus", 
        ".console.output", ".contrasts", ".dataSetLabel", ".dataSetName", ".double.click", ".factors",
        ".length.messages", ".log", ".log.commands", ".logFileName", ".logFont", ".log.font.size", ".log.text.color",
        ".messages", ".messages.connection", 
        ".modelLabel", ".modelName", ".modelNumber", ".modelWithSubset", ".multiple.select.mode",
        ".numeric", "oldPager", ".operatorFont", ".output", ".output.text.color", ".outputFileName", 
        ".report.X11.warnings", ".rgl", ".saveFileName", ".saveOptions", ".sort.names",
        ".twoLevelFactors", ".variables")
    response <- tclvalue(tkmessageBox(message="Exit?",
        icon="question", type="okcancel", default="cancel"))
    if (response == "cancel") return(invisible(response))
    sink(type="message")
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
        response3 <- tkmessageBox(message="Save output file?",
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
    setOption <- function(option, default) {
        if (is.null(current[[option]])) default else current[[option]]
        }
    initializeDialog(title="Commander Options")
    current <- options("Rcmdr")[[1]]
    console.output <- setOption("console.output", FALSE)
    log.commands <- setOption("log.commands", TRUE)
    log.font.size <- setOption("log.font.size", 10)
    log.width <- setOption("log.width", 80)
    log.height <- if (!is.null(current$log.height)) current$log.height
                    else if (!log.commands) 0 else 10
    output.height <- if (!is.null(current$output.height)) current$output.height
        else if (console.output) 0 else 2*log.height 
    contrasts <- setOption("contrasts", c("contr.Treatment", "contr.poly"))
    grab.focus <- setOption("grab.focus", TRUE)
    double.click <- setOption("double.click", FALSE)
    sort.names <- setOption("sort.names", TRUE)
    show.edit.button <- setOption("show.edit.button", TRUE)
    scale.factor <- current$scale.factor
    default.font.size <- setOption("default.font.size", 10)
    default.font <- setOption("default.font", 
        paste("*helvetica-medium-r-normal-*-", default.font.size, "*", sep=""))
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
    OKCancelHelp(helpSubject="Commander")
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
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2)
    }

setOutputWidth <- function(){
    initializeDialog(title="Reset Output Width")
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
    OKCancelHelp(helpSubject="options")
    tkgrid(tklabel(top, text="Output width (characters)"), logWidthSlider, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }
   
