# The R Commander and command logger

# last modified 12 May 04 by J. Fox

Commander <- function(){
    etc <- file.path(.path.package(package="Rcmdr")[1], "etc")
    onCopy <- function(){
        selection <- strsplit(tclvalue(tktag.ranges(.log, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        text <- tclvalue(tkget(.log, selection[1], selection[2]))
        tkclipboard.clear()
        tkclipboard.append(text)
        }    
    onDelete <- function(){
        selection <- strsplit(tclvalue(tktag.ranges(.log, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        tkdelete(.log, selection[1], selection[2])
        }        
    onCut <- function(){
        onCopy()
        onDelete()
        }        
    onPaste <- function(){
        text <- tclvalue(.Tcl("selection get -selection CLIPBOARD"))    
        if (length(text) == 0) return()
        tkinsert(.log, "insert", text)
        }       
    onFind <- function(){
        top <- tktoplevel()
        tkwm.title(top, "Find")
        textFrame <- tkframe(top)
        textVar <- tclVar("")
        textEntry <- tkentry(textFrame, width="20", textvariable=textVar)
        optionsFrame <- tkframe(top)
        regexprVar <- tclVar("0")
        regexprCheckBox <- tkcheckbutton(optionsFrame, variable=regexprVar)
        caseVar <- tclVar("1")
        caseCheckBox <- tkcheckbutton(optionsFrame, variable=caseVar)
        directionVar <- tclVar("-forward")
        forwardButton <- tkradiobutton(optionsFrame, variable=directionVar, value="-forward")
        backwardButton <- tkradiobutton(optionsFrame, variable=directionVar, value="-backward")
        onOK <- function(){
            text <- tclvalue(textVar)
            if (text == ""){
                tkmessageBox(message="No search text specified.", 
                    icon="error", type="ok")
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                onFind()
                return()
                }
            type <- if (tclvalue(regexprVar) == 1) "-regexp" else "-exact"
            case <- tclvalue(caseVar) == 1
            direction <- tclvalue(directionVar)
            stop <- if (direction == "-forward") "end" else "1.0"
            where <- if (case) tksearch(.log, type, direction, "--", text, "insert", stop)
                        else tksearch(.log, type, direction, "-nocase", "--", text, "insert", stop)
            where <- tclvalue(where)
            if (where == "") {
                tkmessageBox(message="Text not found.",
                    icon="info", type="ok")
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                tkfocus(.commander)
                return()
                }
            if (.grab.focus) tkgrab.release(top)
            tkfocus(.log)
            tkmark.set(.log, "insert", where)  
            tksee(.log, where)
            tkdestroy(top)  
            }
        onCancel <- function() {
            if (.grab.focus) tkgrab.release(top)
            tkfocus(.commander)
            tkdestroy(top)  
            }
        buttonsFrame <- tkframe(top)
        OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active")
        cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12",command=onCancel)  
        tkgrid(tklabel(textFrame, text="Search for:"), textEntry, sticky="w") 
        tkgrid(textFrame, sticky="w") 
        tkgrid(tklabel(optionsFrame, text="Regular-expression search"), regexprCheckBox, sticky="w")
        tkgrid(tklabel(optionsFrame, text="Case sensitive"), caseCheckBox, sticky="w")
        tkgrid(tklabel(optionsFrame, text="Search Direction", fg="blue"), sticky="w")
        tkgrid(tklabel(optionsFrame, text="Forward"), forwardButton, sticky="w")
        tkgrid(tklabel(optionsFrame, text="Backward"), backwardButton, sticky="w")
        tkgrid(optionsFrame, sticky="w")
        tkgrid(OKbutton, tklabel(buttonsFrame, text="        "), cancelButton, sticky="w")
        tkgrid(buttonsFrame, sticky="w")
        for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
        for (col in 0:0) tkgrid.columnconfigure(top, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(top, 0, 0)
        tkbind(top, "<Return>", onOK)
        if (.double.click) tkbind(top, "<Double-ButtonPress-1>", onOK)
        tkwm.deiconify(top)
        if (.grab.focus) tkgrab.set(top)
        tkfocus(textEntry)
        tkwait.window(top)
        }    
    onSelectAll <- function() {
        tktag.add(.log, "sel", "1.0", "end")
        tkfocus(.log)
        }
    assign(".activeDataSet", NULL, envir=.GlobalEnv)
    assign(".activeModel", NULL, envir=.GlobalEnv)
    assign(".logFileName", NULL, envir=.GlobalEnv)
    assign(".modelNumber", 0, envir=.GlobalEnv)
    assign(".rgl", FALSE, envir=.GlobalEnv)
    log.font.size <- options("Rcmdr")[[1]]$log.font.size
    log.font.size <- if (is.null(log.font.size)) 10 else log.font.size
    assign(".logFont", tkfont.create(family="courier", size=log.font.size), envir=.GlobalEnv)
    assign(".operatorFont", tkfont.create(family="courier", size=log.font.size), 
        envir=.GlobalEnv)
    scale.factor <- options("Rcmdr")[[1]]$scale.factor
    if (!is.null(scale.factor)) .Tcl(paste("tk scaling ", scale.factor, sep=""))
    contrasts <- options("Rcmdr")[[1]]$contrasts
    contrasts <- if (is.null(contrasts)) c("contr.Treatment", "contr.poly") else contrasts
    assign(".saveOptions", options(warn=1, contrasts=contrasts, 
        na.action="na.exclude", graphics.record=TRUE), envir=.GlobalEnv)
    log.height <- options("Rcmdr")[[1]]$log.height
    log.height <- if (is.null(log.height)) "15" else as.character(log.height)
    log.width <- options("Rcmdr")[[1]]$log.width
    log.width <- if (is.null(log.width)) "70" else as.character(log.width)    
    double.click <- options("Rcmdr")[[1]]$double.click
    assign(".double.click", if (is.null(double.click)) FALSE else double.click, envir=.GlobalEnv)
    sort.names <- options("Rcmdr")[[1]]$sort.names
    assign(".sort.names", if (is.null(sort.names)) TRUE else sort.names, envir=.GlobalEnv)
    grab.focus <- options("Rcmdr")[[1]]$grab.focus
    assign(".grab.focus", if (is.null(grab.focus)) TRUE else grab.focus, envir=.GlobalEnv)
    if (.Platform$OS.type != "windows") {
        assign(".oldPager", options(pager=RcmdrPager), envir=.GlobalEnv)
        default.font.size <- options("Rcmdr")[[1]]$default.font.size
        default.font.size <- if (is.null(default.font.size)) "10" else as.character(default.font.size)
        default.font <- options("Rcmdr")[[1]]$default.font
        default.font <- if (is.null(default.font)) paste("*helvetica-medium-r-normal-*-",
            default.font.size, "*", sep="") else default.font
        .Tcl(paste("option add *font ", default.font, sep=""))
        } 
    assign(".commander", tktoplevel(), envir=.GlobalEnv)
    placement <- options("Rcmdr")[[1]]$placement
    placement <- if (is.null(placement)) "-40+40" else placement
    tkwm.geometry(.commander, placement)
    tkwm.title(.commander, "R Commander")
    tkwm.protocol(.commander, "WM_DELETE_WINDOW", closeCommander)
    topMenu <- tkmenu(.commander)
    tkconfigure(.commander, menu=topMenu)
    .commander.done <<- tclVar("0") # to address problem in Debian Linux
    source.files <- list.files(etc, pattern="*.R$")
    for (file in source.files) {
        source(file.path(etc, file))
        cat(paste("Sourced:", file, "\n"))
        }
    Menus <- read.table(file.path(etc, "Rcmdr-menus.txt"), as.is=TRUE)
    for (m in 1:nrow(Menus)){
        if (Menus[m, 1] == "menu") assign(Menus[m, 2], tkmenu(eval(parse(text=Menus[m, 3])), tearoff=FALSE)) 
        else if (Menus[m, 1] == "item") {
            if (Menus[m, 3] == "command")
                tkadd(eval(parse(text=Menus[m, 2])),"command", label=Menus[m, 4], command=eval(parse(text=Menus[m, 5])))
            else if (Menus[m, 3] == "cascade")
                tkadd(eval(parse(text=Menus[m, 2])),"cascade", label=Menus[m, 4], menu=eval(parse(text=Menus[m, 5])))
            else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
            }
        else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
        }
    exceptions <- scan(file.path(etc, "log-exceptions.txt"), what="", quiet=TRUE, comment.char="#")
    onEdit <- function(){
        if (activeDataSet() == FALSE) {
            tkfocus(.commander)
            return()
            }
        command <- paste("fix(", .activeDataSet, ")", sep="")
        logger(command)
        justDoIt(command)
        activeDataSet(.activeDataSet)
        tkwm.deiconify(.commander)
        tkfocus(.commander)
        }
    onView <- function(){
        if (activeDataSet() == FALSE) {
            tkfocus(.commander)
            return()
            }
        command <- paste("showData(", .activeDataSet, ", placement='-20+200', font=.logFont, maxwidth=", 
            log.width, ", maxheight=", as.numeric(log.height)*2, ")", sep="")
        logger(command)
        justDoIt(command)
        tkwm.deiconify(.commander)
        tkfocus(.commander)
        }
    onSubmit <- function(){
        selection <- strsplit(tclvalue(tktag.ranges(.log, "sel")), " ")[[1]]
        if (is.na(selection[1])) {
            tktag.add(.log, "currentLine", "insert linestart", "insert lineend")
            selection <- strsplit(tclvalue(tktag.ranges(.log, "currentLine")), " ")[[1]]
            tktag.delete(.log, "currentLine")
            if (is.na(selection[1])) {
                tkmessageBox(message=paste("Nothing is selected."),
                    icon="error", type="ok")
                tkfocus(.commander)
                return()
                }
            }
        lines <- tclvalue(tkget(.log, selection[1], selection[2]))
        lines <- strsplit(lines, "\n")[[1]]
        for (line in lines){
            cat(paste("\nR-cmdr>", line, "\n"))
            if (length(grep("<-", line)) > 0){
                var.value <- strsplit(line, "<-")[[1]]
                var <- gsub(" ", "", var.value[1])
                value <- var.value[2]
                if ( (length(grep("\\$", var)) > 0) || (length(grep("\\[", var)) > 0) 
                    || length(grep("\\(", var) > 0))
#                    || length(grep("row.names\\(", var) > 0) 
#                    || length(grep("rownames\\(", var) > 0)
#                    || length(grep("colnames\\(", var) > 0))
#                    justDoIt(paste(var, "<<-", value))
                    justDoIt(paste(var, "<-", value))
                else assign(var, justDoIt(value), envir=.GlobalEnv)
                }
            else if (length(grep("^remove\\(", line)) > 0){
                line <- sub(")", ", envir=.GlobalEnv)", line)
                eval(parse(text=line), envir=.GlobalEnv)
                }
            else if (length(grep("^hist\\(", line)) > 0){ 
                eval(parse(text=paste("plot(", line, ")", sep="")),envir=.GlobalEnv)
                }
            else if (any(sapply(exceptions, 
                    function(.x) length(grep(paste("^", .x, "\\(", sep=""), line)) > 0))){ 
                justDoIt(line)
                }
            else {
                result <- eval(parse(text=line), envir=.GlobalEnv)
                if (!is.null(result)) print(result)
                }
            }
        }
    contextMenu <- function(){
        contextMenu <- tkmenu(tkmenu(.log), tearoff=FALSE)
        tkadd(contextMenu, "command", label="Submit", command=onSubmit)
        tkadd(contextMenu, "command", label="Cut", command=onCut)
        tkadd(contextMenu, "command", label="Copy", command=onCopy)
        tkadd(contextMenu, "command", label="Paste", command=onPaste)
        tkadd(contextMenu, "command", label="Detele", command=onDelete)
        tkadd(contextMenu, "command", label="Find...", command=onFind)
        tkadd(contextMenu, "command", label="Select all", command=onSelectAll)
        tkpopup(contextMenu, tkwinfo("pointerx", .log), tkwinfo("pointery", .log))
        }    
    controlsFrame <- tkframe(.commander)
    editButton <- tkbutton(controlsFrame, text="Edit data set", command=onEdit)
    viewButton <- tkbutton(controlsFrame, text="View data set", command=onView)
    submitButton <- tkbutton(.commander, bitmap=paste("@", file.path(etc, "submit.xbm"), sep=""), 
        borderwidth="2", command=onSubmit)
    assign(".logCommands", tclVar("1"), envir=.GlobalEnv)
    logCheckBox <- tkcheckbutton(controlsFrame, variable=.logCommands)
    assign(".attachDataSet", tclVar("1"), envir=.GlobalEnv)
    attachCheckBox <- tkcheckbutton(controlsFrame, variable=.attachDataSet)
    assign(".dataSetName", tclVar("<No active dataset>"), envir=.GlobalEnv)
    assign(".dataSetLabel", tkbutton(controlsFrame, textvariable=.dataSetName, fg="red",
        relief="groove", command=selectActiveDataSet),
        envir=.GlobalEnv)
    logFrame <- tkframe(.commander)
    assign(".log", tktext(logFrame, bg="white", font=.logFont, 
        height=log.height, width=log.width, wrap="none"),  envir=.GlobalEnv)
    logXscroll <- tkscrollbar(logFrame, repeatinterval=5, orient="horizontal",
        command=function(...) tkxview(.log, ...))
    logYscroll <- tkscrollbar(logFrame, repeatinterval=5,
        command=function(...) tkyview(.log, ...))
    tkconfigure(.log, xscrollcommand=function(...) tkset(logXscroll, ...))
    tkconfigure(.log, yscrollcommand=function(...) tkset(logYscroll, ...))
    assign(".modelName", tclVar("<No active model>"), envir=.GlobalEnv)
    bottomLeftFrame <- tkframe(.commander)
    assign(".modelLabel", tkbutton(bottomLeftFrame, textvariable=.modelName, fg="red",
        relief="groove", command=selectActiveModel), 
        envir=.GlobalEnv)
    show.edit.button <- options("Rcmdr")[[1]]$show.edit.button
    show.edit.button <- if (is.null(show.edit.button)) TRUE else show.edit.button
    tkgrid(tklabel(controlsFrame, bitmap=paste("@", file.path(etc, "Rcmdr.xbm"), sep=""), fg="red"), 
        tklabel(controlsFrame, text="Data set:"), .dataSetLabel, 
        tklabel(controlsFrame, text="  "), if(show.edit.button) editButton, viewButton, 
        tklabel(controlsFrame, text="  Log commands:"), logCheckBox, 
        tklabel(controlsFrame, text="  Attach active data set:"), attachCheckBox, sticky="w")
    tkgrid(controlsFrame, sticky="w", columnspan="2")
    tkgrid(.log, logYscroll)
    tkgrid(logXscroll)
    tkgrid(logFrame, columnspan="2")
    tkgrid(tklabel(bottomLeftFrame, text="Model: "), .modelLabel)
    tkgrid(bottomLeftFrame, submitButton)
    tkgrid.configure(logYscroll, sticky="ns")
    tkgrid.configure(logXscroll, sticky="ew")
    tkgrid.configure(submitButton, sticky="e")
    tkgrid.configure(bottomLeftFrame, sticky="w")
    for (row in 0:4) tkgrid.rowconfigure(.commander, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(.commander, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(.commander, 0, 0)
    tkbind(.commander, "<Control-r>", onSubmit)
    tkbind(.commander, "<Control-R>", onSubmit)
    tkbind(.commander, "<Control-f>", onFind)
    tkbind(.commander, "<Control-F>", onFind)
    tkbind(.log, "<ButtonPress-3>", contextMenu)
    tkwm.deiconify(.commander)
    tkfocus(.commander)
    tkwait <- options("Rcmdr")[[1]]$tkwait  # to address problem in Debian Linux
    if ((!is.null(tkwait)) && tkwait) tkwait.variable(.commander.done)
    }

logger <- function(command){
    if (tclvalue(.logCommands) == "1") {
        tkinsert(.log, "end", paste(command,"\n", sep=""))
        tkyview.moveto(.log, 1)
        }
    lines <- strsplit(command, "\n")[[1]]
    for (line in lines) cat(paste("\nR-cmdr>", line, "\n"))
    command
    }

doItAndPrint <- function(command) {
    result <- try(eval(parse(text=logger(command)), envir=.GlobalEnv), 
        silent=TRUE)
    if (class(result)[1] ==  "try-error"){
        tkmessageBox(message=paste("Error:",
            strsplit(result, ":")[[1]][2]), icon="error")
        tkfocus(.commander)
        return()
        }
    if (!is.null(result)) print(result)
    }

justDoIt <- function(command) {
    result <- try(eval(parse(text=command), envir=.GlobalEnv), silent=TRUE)
    if (class(result)[1] ==  "try-error"){
        tkmessageBox(message=paste("Error:",
            strsplit(result, ":")[[1]][2]), icon="error")
        tkfocus(.commander)
        return()
        }
    result
    }
    
