# The R Commander and command logger

# last modified 7 August 03 by J. Fox

Commander <- function(){
    etc <- file.path(.path.package(package="Rcmdr")[1], "etc")
    assign(".activeDataSet", NULL, envir=.GlobalEnv)
    assign(".activeModel", NULL, envir=.GlobalEnv)
    assign(".logFileName", NULL, envir=.GlobalEnv)
    assign(".modelNumber", 0, envir=.GlobalEnv)
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
        na.action="na.exclude"), envir=.GlobalEnv)
    log.height <- options("Rcmdr")[[1]]$log.height
    log.height <- if (is.null(log.height)) "15" else as.character(log.height)
    log.width <- options("Rcmdr")[[1]]$log.width
    log.width <- if (is.null(log.width)) "70" else as.character(log.width)    
    double.click <- options("Rcmdr")[[1]]$double.click
    assign(".double.click", if (is.null(double.click)) FALSE else double.click, envir=.GlobalEnv)
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
        command <- paste("fix(", .activeDataSet, ")", sep="")
        logger(command)
        justDoIt(command)
        activeDataSet(.activeDataSet)
        tkwm.deiconify(.commander)
        tkfocus(.commander)
        }
    onView <- function(){
        command <- paste("edit(", .activeDataSet, ")", sep="")
        logger(paste("invisible(", command, ")", sep=""))
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
                    || length(grep("row.names\\(", var) > 0) )
                    justDoIt(paste(var, "<<-", value))
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
    tkgrid(tklabel(controlsFrame, bitmap=paste("@", file.path(etc, "Rcmdr.xbm"), sep=""), fg="red"), 
        tklabel(controlsFrame, text="Data set:"), .dataSetLabel, 
        tklabel(controlsFrame, text="  "), editButton, viewButton, 
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
    for (row in 0:3) tkgrid.rowconfigure(.commander, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(.commander, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(.commander, 0, 0)
    tkbind(.commander, "<Control-r>", onSubmit)
    tkbind(.commander, "<Control-R>", onSubmit)
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
    print(result)
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
    
