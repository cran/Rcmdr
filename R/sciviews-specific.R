# SciViews specific R Commander code

# last modified 24 October 2004 by Ph. Grosjean

is.SciViews <- function() {
    # SciViews defines the option "SciViews.version".
    # So, we test if we are in SciViews this way:
    res <- !is.null(getOption("SciViews.version"))
    res
    }
    
is.SciViews.TclTk <- function() {
    # Determine if a TclTk-communicating SciViews client is currently running
    res <- (!is.null(getOption("SciViews.TclTk")) && getOption("SciViews.TclTk") == TRUE)
    res
    }
    
tkfocus <- function(...){
    # A call to tcltk:::tkfocus() causes a GPF in SciViews
    # => replaced by this version that check if we are in SciViews or not
    if (!is.SciViews()) tcltk:::tkfocus(...)
    }

svCommander <- function(){
    # The SciViews specific Commander() function
    if (is.SciViews()) {
        ### TO DO: automatically generate the menu from "Rcmdr-menus.txt"
        # Display the R commander menu
        #...
        setOption <- function(option, default, global=TRUE) {
            opt <- if (is.null(current[[option]])) default else current[[option]]
            if (global) assign(paste(".", option, sep=""), opt, envir=.GlobalEnv)
            else opt
            }
        etc <- file.path(.path.package(package="Rcmdr")[1], "etc")
        # Do NOT sink error messages!
        #assign(".messages.connection", textConnection(".messages", open = "w"), envir=.GlobalEnv)
        #sink(.messages.connection, type="message")
        assign(".length.messages", 0, envir=.GlobalEnv)
        assign(".activeDataSet", NULL, envir=.GlobalEnv)
        assign(".activeModel", NULL, envir=.GlobalEnv)
        assign(".logFileName", NULL, envir=.GlobalEnv)
        assign(".outputFileName", NULL, envir=.GlobalEnv)
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
        assign(".logCommands", if (.log.commands) tclVar("1") else tclVar("0"), envir=.GlobalEnv)
        setOption("console.output", TRUE) # Must be set to TRUE for SciViews app!
        log.height <- as.character(setOption("log.height", if (!.log.commands) 0 else 10, global=FALSE))
        log.width <- as.character(setOption("log.width", 80, global=FALSE))
        output.height <- as.character(setOption("output.height",
            if (.console.output) 0
            else if ((as.numeric(log.height) != 0) || (!.log.commands)) 2*as.numeric(log.height)
            else 20, global=FALSE))
        setOption("output.height", output.height)
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
        placement <- setOption("placement", "-40+40", global=FALSE)
        source.files <- list.files(etc, pattern="*.R$")
        .commander.done <<- tclVar("0") # to address problem in Debian Linux
        source.files <- list.files(etc, pattern="*.R$")
        for (file in source.files) {
             source(file.path(etc, file))
             cat(paste("Sourced:", file, "\n"))
             }
        Menus <- read.table(file.path(etc, "Rcmdr-menus.txt"), as.is=TRUE)
        ### TO DO: we need another treatment for this!
        #for (m in 1:nrow(Menus)){
        #    if (Menus[m, 1] == "menu") assign(Menus[m, 2], tkmenu(eval(parse(text=Menus[m, 3])), tearoff=FALSE))
        #    else if (Menus[m, 1] == "item") {
        #         if (Menus[m, 3] == "command")
        #             tkadd(eval(parse(text=Menus[m, 2])),"command", label=Menus[m, 4], command=eval(parse(text=Menus[m, 5])))
        #         else if (Menus[m, 3] == "cascade")
        #             tkadd(eval(parse(text=Menus[m, 2])),"cascade", label=Menus[m, 4], menu=eval(parse(text=Menus[m, 5])))
        #         else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
        #         }
        #    else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
        #    }
        exceptions <- scan(file.path(etc, "log-exceptions.txt"), what="", quiet=TRUE, comment.char="#")
        assign(".dataSetName", tclVar("<No active dataset>"), envir=.GlobalEnv)
        assign(".modelName", tclVar("<No active model>"), envir=.GlobalEnv)
        show.edit.button <- options("Rcmdr")[[1]]$show.edit.button
        show.edit.button <- if (is.null(show.edit.button)) TRUE else show.edit.button
        }
    }

svlogger <- function(command){
    # the SciViews specific logger() function
    if (is.SciViews()) {
        # Behaviour is different if it is a TclTk communicating client,
        # or a plug
        if (is.SciViews.TclTk()) { # TclTk SciViews client
            # getTemp from SciViews[svMisc] is redefined here to avoid a Depends: svMisc!
            getTemp <- function(x, default = NULL, mode="any") {
                if  (exists(x, envir = TempEnv(), mode = mode, inherits = FALSE)) {
                    return(get(x, envir = TempEnv(), mode = mode, inherits = FALSE))
                } else { # Variable not found, return the default value
                    return(default)
                }
            }
            if (tclvalue(.logCommands) == "1") {
                CmdFun <- getTemp(".guiCmd", mode = "function")
                if (!is.null(CmdFun)) CmdFun(paste("«Log\n", command))
            }
            lines <- strsplit(command, "\n")[[1]]
            for (line in lines) cat(paste("\n» Rcmdr »", line, "\n"))
            command
        } else {    # plug SciViews client
            lines <- strsplit(command, "\n")[[1]]
            for (line in lines) {
                cat(paste("®History", line, "®\n¯ ", line, "    #[R-cmdr]\n", sep=""))
                if (tclvalue(.logCommands) == "1") cat(paste("®Script", command, "\n®", sep=""))
                }
            command
            }
        }
    }

activeDataSetEdit <- function() {
    # This is SciViews equivalent to onEdit function of Commander()
    if (activeDataSet() == FALSE)
        return()
    command <- paste("fix(", .activeDataSet, ")", sep="")
    justDoIt(command)
    svlogger(command)
    activeDataSet(.activeDataSet)
    invisible()
}

activeDataSetView <- function() {
    # This is SciViews equivalent to onView function of Commander()
    if (activeDataSet() == FALSE)
        return()
    view.height <- 30 #max(as.numeric(output.height) + as.numeric(log.height), 10)
    command <- paste("showData(", .activeDataSet, ", placement='-20+200', font=.logFont, maxwidth=", 
    80, ", maxheight=", view.height, ")", sep="")
    justDoIt(command)
    invisible(svlogger(command))
    }

optionLogCommand <- function() {
    # Change log option in SciViews
    response <- tclvalue(tkmessageBox(message="Log R-cmdr commands in a script?",
        icon="question", type="yesno", default="yes"))
    val <- if (response == "yes") tclVar("1") else tclVar("0")
    assign(".logCommands", val, envir=.GlobalEnv)
    val <- if (response == "yes") TRUE else FALSE
    assign(".log.commands", val, envir=.GlobalEnv)
    Opts <- options("Rcmdr")[[1]]
    Opts$log.commands <- val
    options(Rcmdr=Opts)
    refreshStatus()
}

optionAttachDataSet <- function() {
    # Change attach option in SciViews
    response <- tclvalue(tkmessageBox(message="Attach active data set?",
        icon="question", type="yesno", default="yes"))
    val <- if (response == "yes") TRUE else FALSE
    assign(".attach.data.set", val, envir=.GlobalEnv)
    Opts <- options("Rcmdr")[[1]]
    Opts$attach.data.set <- val
    options(Rcmdr=Opts)
    refreshStatus()
}

optionSortVariables <- function() {
    # Change sort variable names option
    response <- tclvalue(tkmessageBox(message="Sort variable names alphabetically?",
        icon="question", type="yesno", default="yes"))
    val <- if (response == "yes") TRUE else FALSE
    assign(".sort.names", val, envir=.GlobalEnv)
    Opts <- options("Rcmdr")[[1]]
    Opts$sort.names <- val
    options(Rcmdr=Opts)
    refreshStatus()
}

refreshStatus <- function() {
    # Refresh dataset and model indication in the status bar of SciViews Client
    DataSet <- get(".activeDataSet", pos = .GlobalEnv)
    if (is.null(DataSet) || length(DataSet) == 0) DataSet <- "<no data>"
    Model <- get(".activeModel", pos = .GlobalEnv)
    if (is.null(Model) || length(Model) == 0) Model <- "<no model>"
    if (get(".log.commands", pos = .GlobalEnv)) Opts <- " [log]" else Opts <- " "
    if (get(".attach.data.set", pos = .GlobalEnv)) Opts <- paste(Opts, "[attach]", sep="")
    if (get(".sort.names", pos = .GlobalEnv)) Opts <- paste(Opts, "[sort]", sep="")
    # If it is a "SciViews TclTk GUI" client, use it...
    if (is.SciViews.TclTk()) {
        cat(paste("Data: ", DataSet, ", Model: ", Model, Opts, sep=""), file = file.path(tempdir(), "svStatus.txt"))
        # getTemp from SciViews[svMisc] is redefined here to avoid a Depends: svMisc!
        getTemp <- function(x, default = NULL, mode="any") {
            if  (exists(x, envir = TempEnv(), mode = mode, inherits = FALSE)) {
                return(get(x, envir = TempEnv(), mode = mode, inherits = FALSE))
            } else { # Variable not found, return the default value
                return(default)
            }
        }
        CmdFun <- getTemp(".guiCmd", mode = "function")
        if (!is.null(CmdFun)) CmdFun("«Status")
        # Possibly update data in the object browser
        if (exists(".guiObjCallback", envir = TempEnv(), inherits = FALSE)) getTemp(".guiObjCallback")()
    } else {    # This should be SciViews Insider, or other similar client
        cat("®StatusTextData: ", DataSet, ", Model: ", Model, Opts, "®", sep="")
    }
}
