# The R Commander and command logger

# last modified 30 July 04 by J. Fox

Commander <- function(){
    setOption <- function(option, default, global=TRUE) {
        opt <- if (is.null(current[[option]])) default else current[[option]]
        if (global) assign(paste(".", option, sep=""), opt, envir=.GlobalEnv)
        else opt
        }
    etc <- file.path(.path.package(package="Rcmdr")[1], "etc")
    onCopy <- function(){
        focused <- tkfocus()
        if ((tclvalue(focused) != .log$ID) && (tclvalue(focused) != .output$ID)) 
            focused <- .log
        selection <- strsplit(tclvalue(tktag.ranges(focused, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        text <- tclvalue(tkget(focused, selection[1], selection[2]))
        tkclipboard.clear()
        tkclipboard.append(text)
        }    
    onDelete <- function(){
        focused <- tkfocus()
        if ((tclvalue(focused) != .log$ID) && (tclvalue(focused) != .output$ID))  
            focused <- .log
        selection <- strsplit(tclvalue(tktag.ranges(focused, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        tkdelete(focused, selection[1], selection[2])
        }        
    onCut <- function(){
        onCopy()
        onDelete()
        }        
    onPaste <- function(){
        focused <- tkfocus()
        if ((tclvalue(focused) != .log$ID) && (tclvalue(focused) != .output$ID))  
            focused <- .log
        text <- tclvalue(.Tcl("selection get -selection CLIPBOARD"))    
        if (length(text) == 0) return()
        tkinsert(focused, "insert", text)
        }       
    onFind <- function(){
        focused <- tkfocus()
        if ((tclvalue(focused) != .log$ID) && (tclvalue(focused) != .output$ID))  
            focused <- .log
        initializeDialog(title="Find")
        textFrame <- tkframe(top)
        textVar <- tclVar("")
        textEntry <- tkentry(textFrame, width="20", textvariable=textVar)
        checkBoxes(frame="optionsFrame", boxes=c("regexpr", "case"), initialValues=c("0", "1"), 
            labels=c("Regular-expression search", "Case sensitive"))
        radioButtons(name="direction", buttons=c("foward", "backward"), labels=c("Forward", "Backward"),
            values=c("-forward", "-backward"), title="Search Direction")
        onOK <- function(){
            text <- tclvalue(textVar)
            if (text == ""){
                errorCondition(recall=onFind, message="No search text specified.")
                return()
                }
            type <- if (tclvalue(regexprVariable) == 1) "-regexp" else "-exact"
            case <- tclvalue(caseVariable) == 1
            direction <- tclvalue(directionVariable)
            stop <- if (direction == "-forward") "end" else "1.0"
            where <- if (case) tksearch(focused, type, direction, "--", text, "insert", stop)
                        else tksearch(focused, type, direction, "-nocase", "--", text, "insert", stop)
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
            tkfocus(focused)
            tkmark.set(focused, "insert", where)  
            tksee(focused, where)
            tkdestroy(top)  
            }
        OKCancelHelp()
        tkgrid(tklabel(textFrame, text="Search for:"), textEntry, sticky="w") 
        tkgrid(textFrame, sticky="w") 
        tkgrid(optionsFrame, sticky="w")
        tkgrid(directionFrame, sticky="w")
        tkgrid(buttonsFrame, sticky="w")
        dialogSuffix(rows=4, columns=1, focus=textEntry)
         }    
    onSelectAll <- function() {
        focused <- tkfocus()
        if ((tclvalue(focused) != .log$ID) && (tclvalue(focused) != .output$ID))  
            focused <- .log
        tktag.add(focused, "sel", "1.0", "end")
        tkfocus(focused)
        }
    onClear <- function(){
        onSelectAll()
        onDelete()
        }
    assign(".messages.connection", textConnection(".messages", open = "w"), envir=.GlobalEnv)
    sink(.messages.connection, type="message")
    assign(".length.messages", 0, envir=.GlobalEnv)
    assign(".activeDataSet", NULL, envir=.GlobalEnv)
    assign(".activeModel", NULL, envir=.GlobalEnv)
    assign(".logFileName", NULL, envir=.GlobalEnv)
    assign(".outputFileName", NULL, envir=.GlobalEnv)
    assign(".saveFileName", NULL, envir=.GlobalEnv)
    assign(".modelNumber", 0, envir=.GlobalEnv)
    assign(".rgl", FALSE, envir=.GlobalEnv)
    current <- options("Rcmdr")[[1]]
    setOption("log.font.size", 10)
    assign(".logFont", tkfont.create(family="courier", size=.log.font.size), envir=.GlobalEnv)
    assign(".operatorFont", tkfont.create(family="courier", size=.log.font.size), 
        envir=.GlobalEnv)
    scale.factor <- current$scale.factor
    if (!is.null(scale.factor)) .Tcl(paste("tk scaling ", scale.factor, sep=""))
    setOption("contrasts", c("contr.Treatment", "contr.poly"))
    setOption("log.commands", TRUE)
    setOption("console.output", FALSE)
    log.height <- as.character(setOption("log.height", if (!.log.commands) 0 else 10, global=FALSE))
    log.width <- as.character(setOption("log.width", 80, global=FALSE))
    output.height <- as.character(setOption("output.height",
        if (.console.output) 0
        else if ((as.numeric(log.height) != 0) || (!.log.commands)) 2*as.numeric(log.height)
        else 20, global=FALSE))
    assign(".saveOptions", options(warn=1, contrasts=.contrasts, width=as.numeric(log.width),
        na.action="na.exclude", graphics.record=TRUE), envir=.GlobalEnv) 
    setOption("double.click", FALSE)
    setOption("sort.names", TRUE)
    setOption("grab.focus", TRUE)
    setOption("attach.data.set", TRUE)
    setOption("log.text.color", "black")
    setOption("command.text.color", "red")
    setOption("output.text.color", "darkblue")
    setOption("multiple.select.mode", "extended")
    if (.Platform$OS.type != "windows") {
        assign(".oldPager", options(pager=RcmdrPager), envir=.GlobalEnv)
        default.font.size <- as.character(setOption("default.font.size", 10, global=FALSE))
        default.font <- setOption("default.font", 
            paste("*helvetica-medium-r-normal-*-", default.font.size, "*", sep=""), global=FALSE)
        .Tcl(paste("option add *font ", default.font, sep=""))
        } 
    assign(".commander", tktoplevel(), envir=.GlobalEnv)
    placement <- setOption("placement", "-40+40", global=FALSE)
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
        view.height <- max(as.numeric(output.height) + as.numeric(log.height), 10)
        command <- paste("showData(", .activeDataSet, ", placement='-20+200', font=.logFont, maxwidth=", 
            log.width, ", maxheight=", view.height, ")", sep="")
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
        if (!.console.output) tkinsert(.output, "end", "\n")
        iline <- 1
        nlines <- length(lines)
        while (iline <= nlines){
            current.line <- lines[iline]
            if (.console.output) cat(paste("\nRcmdr> ", current.line,"\n", sep=""))
            else{
                tkinsert(.output, "end", paste("> ", current.line,"\n", sep=""))
                tktag.add(.output, "currentLine", "end - 2 lines linestart", "end - 2 lines lineend")
                tktag.configure(.output, "currentLine", foreground=.command.text.color)
                }
            jline <- iline + 1
            while (jline <= nlines){
                if (length(grep("^[\\ \t]", lines[jline])) == 0) break
                if (.console.output)cat(paste("Rcmdr+ ", lines[jline],"\n", sep=""))
                else{
                    tkinsert(.output, "end", paste("+ ", lines[jline],"\n", sep=""))
                    tktag.add(.output, "currentLine", "end - 2 lines linestart", "end - 2 lines lineend")
                    tktag.configure(.output, "currentLine", foreground=.command.text.color)
                    }
                current.line <- paste(current.line, lines[jline])
                jline <- jline + 1
                iline <- iline + 1
                }
            if (length(grep("<-", current.line)) > 0){
                var.value <- strsplit(current.line, "<-")[[1]]
                var <- gsub(" ", "", var.value[1])
                value <- var.value[2]
                if ( (length(grep("\\$", var)) > 0) || (length(grep("\\[", var)) > 0) 
                    || length(grep("\\(", var) > 0))
                    justDoIt(paste(var, "<-", value))
                else assign(var, justDoIt(value), envir=.GlobalEnv)
                }
            else if (length(grep("^remove\\(", current.line)) > 0){
                current.line <- sub(")", ", envir=.GlobalEnv)", current.line)
                justDoIt(current.line)
                }
            else if (length(grep("^hist\\(", current.line)) > 0){ 
                justDoIt(paste("plot(", current.line, ")", sep=""))
                }
            else if (any(sapply(exceptions, 
                    function(.x) length(grep(paste("^", .x, "\\(", sep=""), current.line)) > 0))){ 
                justDoIt(current.line)
                }
            else {
                result <- try(eval(parse(text=current.line), envir=.GlobalEnv), 
                    silent=TRUE)
                if (class(result)[1] ==  "try-error"){
                    tkmessageBox(message=paste("Error:",
                        strsplit(result, ":")[[1]][2]), icon="error")
                    tkfocus(.commander)
                    }
                else if (!is.null(result)) {
                    output <- capture.output(print(result))
                    if (.console.output) for (line in output) cat(paste(line, "\n", sep=""))
                    else {
                        for (line in output) tkinsert(.output, "end", paste(line, "\n", sep=""))
                        tkyview.moveto(.output, 1)
                        }
                    }
                checkWarnings()
                }
            iline <- iline + 1
            }
        tkyview.moveto(.output, 1)
        }
    contextMenuLog <- function(){
        contextMenu <- tkmenu(tkmenu(.log), tearoff=FALSE)
        tkadd(contextMenu, "command", label="Clear window", command=onClear)
        tkadd(contextMenu, "command", label="Submit", command=onSubmit)
        tkadd(contextMenu, "command", label="Cut", command=onCut)
        tkadd(contextMenu, "command", label="Copy", command=onCopy)
        tkadd(contextMenu, "command", label="Paste", command=onPaste)
        tkadd(contextMenu, "command", label="Delete", command=onDelete)
        tkadd(contextMenu, "command", label="Find...", command=onFind)
        tkadd(contextMenu, "command", label="Select all", command=onSelectAll)
        tkpopup(contextMenu, tkwinfo("pointerx", .log), tkwinfo("pointery", .log))
        }  
    contextMenuOutput <- function(){
        contextMenu <- tkmenu(tkmenu(.output), tearoff=FALSE)
        tkadd(contextMenu, "command", label="Clear window", command=onClear)
        tkadd(contextMenu, "command", label="Cut", command=onCut)
        tkadd(contextMenu, "command", label="Copy", command=onCopy)
        tkadd(contextMenu, "command", label="Paste", command=onPaste)
        tkadd(contextMenu, "command", label="Delete", command=onDelete)
        tkadd(contextMenu, "command", label="Find...", command=onFind)
        tkadd(contextMenu, "command", label="Select all", command=onSelectAll)
        tkpopup(contextMenu, tkwinfo("pointerx", .output), tkwinfo("pointery", .output))
        }    
    controlsFrame <- tkframe(.commander)
    editButton <- tkbutton(controlsFrame, text="Edit data set", command=onEdit)
    viewButton <- tkbutton(controlsFrame, text="View data set", command=onView)
    submitButton <- tkbutton(.commander, bitmap=paste("@", file.path(etc, "submit.xbm"), sep=""), 
        borderwidth="2", command=onSubmit)
    assign(".dataSetName", tclVar("<No active dataset>"), envir=.GlobalEnv)
    assign(".dataSetLabel", tkbutton(controlsFrame, textvariable=.dataSetName, fg="red",
        relief="groove", command=selectActiveDataSet),
        envir=.GlobalEnv)
    logFrame <- tkframe(.commander)
    assign(".log", tktext(logFrame, bg="white", fg=.log.text.color, font=.logFont, 
        height=log.height, width=log.width, wrap="none"),  envir=.GlobalEnv)
    logXscroll <- tkscrollbar(logFrame, repeatinterval=5, orient="horizontal",
        command=function(...) tkxview(.log, ...))
    logYscroll <- tkscrollbar(logFrame, repeatinterval=5,
        command=function(...) tkyview(.log, ...))
    tkconfigure(.log, xscrollcommand=function(...) tkset(logXscroll, ...))
    tkconfigure(.log, yscrollcommand=function(...) tkset(logYscroll, ...))
    outputFrame <- tkframe(.commander)
    assign(".output", tktext(outputFrame, bg="white", fg=.output.text.color, font=.logFont, 
        height=output.height, width=log.width, wrap="none"),  envir=.GlobalEnv)
    outputXscroll <- tkscrollbar(outputFrame, repeatinterval=5, orient="horizontal",
        command=function(...) tkxview(.output, ...))
    outputYscroll <- tkscrollbar(outputFrame, repeatinterval=5,
        command=function(...) tkyview(.output, ...))
    tkconfigure(.output, xscrollcommand=function(...) tkset(outputXscroll, ...))
    tkconfigure(.output, yscrollcommand=function(...) tkset(outputYscroll, ...))    
    assign(".modelName", tclVar("<No active model>"), envir=.GlobalEnv)
    assign(".modelLabel", tkbutton(controlsFrame, textvariable=.modelName, fg="red",
        relief="groove", command=selectActiveModel), 
        envir=.GlobalEnv)
    show.edit.button <- options("Rcmdr")[[1]]$show.edit.button
    show.edit.button <- if (is.null(show.edit.button)) TRUE else show.edit.button
    tkgrid(tklabel(controlsFrame, bitmap=paste("@", file.path(etc, "Rcmdr.xbm"), sep=""), fg="red"), 
        tklabel(controlsFrame, text="Data set:"), .dataSetLabel, 
        tklabel(controlsFrame, text="  "), if(show.edit.button) editButton, viewButton, 
        tklabel(controlsFrame, text="    Model: "), .modelLabel, sticky="w")
    tkgrid(controlsFrame, if (.log.commands) submitButton, sticky="w")
    if (.log.commands) tkgrid.configure(submitButton, sticky="e")
    tkgrid(.log, logYscroll, sticky="news")
    tkgrid(logXscroll)
    if (.log.commands) tkgrid(logFrame, sticky="news", padx=10, pady=10, columnspan=2)
    tkgrid(.output, outputYscroll, sticky="news")
    tkgrid(outputXscroll)
    if (!.console.output) tkgrid(outputFrame, sticky="news", padx=10, pady=10, columnspan=2)   
    tkgrid.configure(logYscroll, sticky="ns")
    tkgrid.configure(logXscroll, sticky="ew")   
    tkgrid.configure(outputYscroll, sticky="ns")
    tkgrid.configure(outputXscroll, sticky="ew")    
    tkgrid.rowconfigure(.commander, 0, weight=0)
    tkgrid.rowconfigure(.commander, 1, weight=1)
    tkgrid.rowconfigure(.commander, 2, weight=1)
    tkgrid.columnconfigure(.commander, 0, weight=1)
    tkgrid.columnconfigure(.commander, 1, weight=0)
    if (.log.commands){
        tkgrid.rowconfigure(logFrame, 0, weight=1)
        tkgrid.rowconfigure(logFrame, 1, weight=0)
        tkgrid.columnconfigure(logFrame, 0, weight=1)
        tkgrid.columnconfigure(logFrame, 1, weight=0)
        }
    if (!.console.output){
        tkgrid.rowconfigure(outputFrame, 0, weight=1)
        tkgrid.rowconfigure(outputFrame, 1, weight=0)
        tkgrid.columnconfigure(outputFrame, 0, weight=1)
        tkgrid.columnconfigure(outputFrame, 1, weight=0)
        }    
    .Tcl("update idletasks")
    tkbind(.commander, "<Control-r>", onSubmit)
    tkbind(.commander, "<Control-R>", onSubmit)
    tkbind(.commander, "<Control-f>", onFind)
    tkbind(.commander, "<Control-F>", onFind)
    tkbind(.log, "<ButtonPress-3>", contextMenuLog)
    tkbind(.output, "<ButtonPress-3>", contextMenuOutput)
    tkwm.deiconify(.commander)
    tkfocus(.commander)
    tkwait <- options("Rcmdr")[[1]]$tkwait  # to address problem in Debian Linux
    if ((!is.null(tkwait)) && tkwait) tkwait.variable(.commander.done)
    }

logger <- function(command){
    if (.log.commands) {
        tkinsert(.log, "end", paste(command,"\n", sep=""))
        tkyview.moveto(.log, 1)
        }
    lines <- strsplit(command, "\n")[[1]]
    tkinsert(.output, "end", "\n")
    if (.console.output) for (line in lines) cat(paste("\nRcmdr>", line, "\n"))
    else {
        for (line in lines) tkinsert(.output, "end", paste("> ", command,"\n", sep=""))
        tktag.add(.output, "currentLine", "end - 2 lines linestart", "end - 2 lines lineend")
        tktag.configure(.output, "currentLine", foreground=.command.text.color)
        tkyview.moveto(.output, 1)
        }
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
    if (!is.null(result)) {
        if (.console.output) print(result)
        else{
            output <- capture.output(print(result))
            for (line in output) tkinsert(.output, "end", paste(line, "\n", sep=""))
            tkyview.moveto(.output, 1)
            }
        }
    checkWarnings()
    result
    }

justDoIt <- function(command) {
    result <- try(eval(parse(text=command), envir=.GlobalEnv), silent=TRUE)
    if (class(result)[1] ==  "try-error"){
        tkmessageBox(message=paste("Error:",
            strsplit(result, ":")[[1]][2]), icon="error")
        tkfocus(.commander)
        return()
        }
    checkWarnings()
    result
    }
    
checkWarnings <- function(){
    length.messages <- length(.messages)
    if (length.messages > .length.messages){
        tkmessageBox(message=paste(.messages[(.length.messages + 1):length.messages], collapse="\n"), 
            icon="warning")
        assign(".length.messages", length.messages, envir=.GlobalEnv)
        tkfocus(.commander)
        }
    }
