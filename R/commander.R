# The R Commander and command logger

# last modified 23 May 03 by J. Fox

Commander <- function(){
    if (.Platform$OS.type != "windows") assign(".oldPager", options(pager=tkpager), envir=.GlobalEnv)
    assign(".saveOptions", options(warn=1, contrasts=c("contr.Treatment", "contr.poly"), 
        na.action="na.exclude"), envir=.GlobalEnv)
    options()
    assign(".commander", tktoplevel(), envir=.GlobalEnv)
    tkwm.title(.commander, "R Commander")
    tkwm.protocol(.commander, "WM_DELETE_WINDOW", closeCommander)
    topMenu <- tkmenu(.commander)
    tkconfigure(.commander, menu=topMenu)
    Menus <- read.table(paste(.menus, "Rcmdr-menus.txt", sep=""), as.is=TRUE)
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
    onEdit <- function(){
        justDoIt(paste("fix(", .activeDataSet, ")", sep=""))
        activeDataSet(.activeDataSet)
        tkfocus(.commander)
        }
    onView <- function(){
        justDoIt(paste("edit(", .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        }
    onSubmit <- function(){
        selection <- strsplit(tclvalue(tktag.ranges(.log, "sel")), " ")[[1]]
        if (is.na(selection[1])) {
            tkmessageBox(message=paste("Nothing is selected."),
                icon="error", type="ok")
            return()
            }
        lines <- tclvalue(tkget(.log, selection[1], selection[2]))
        lines <- strsplit(lines, "\n")[[1]]
        for (line in lines){
            cat(paste("\nR-cmdr>", line, "\n"))
            if (length(grep("<-", line)) > 0){
                var.value <- strsplit(line, "<-")[[1]]
                var <- gsub(" ", "", var.value[1])
                value <- var.value[2]
                assign(var, justDoIt(value), envir=.GlobalEnv)
                }
            else if (length(grep("^remove\\(", line)) > 0){
                line <- sub(")", ", envir=.GlobalEnv)", line)
                eval(parse(text=line), envir=.GlobalEnv)
                }
            else if (length(grep("^hist\\(", line)) > 0){ 
                eval(parse(text=paste("plot(", line, ")", sep="")),envir=.GlobalEnv)
                }
            else if (length(grep("^scatterplot.matrix\\(", line)) > 0){ 
                justDoIt(line)
                }
            else if (length(grep("^barplot\\(", line)) > 0){ 
                justDoIt(line)
                }
            else if (length(grep("^boxplot\\(", line)) > 0){ 
                justDoIt(line)
                }
            else if (length(grep("^attach\\(", line)) > 0){ 
                justDoIt(line)
                }
            else if (length(grep("^data\\(", line)) > 0){ 
                justDoIt(line)
                }
            else if (length(grep("^print\\(", line)) > 0){ 
                justDoIt(line)
                }
            else print(eval(parse(text=line), envir=.GlobalEnv))
            }
        }
    controlsFrame <- tkframe(.commander)
    editButton <- tkbutton(controlsFrame, text="Edit data set", command=onEdit)
    viewButton <- tkbutton(controlsFrame, text="View data set", command=onView)
    submitButton <- tkbutton(.commander, bitmap=paste("@", .images, "submit.xbm", sep=""), 
        borderwidth="2", command=onSubmit)
    assign(".logCommands", tclVar("1"), envir=.GlobalEnv)
    logCheckBox <- tkcheckbutton(controlsFrame, variable=.logCommands)
    assign(".dataSetName", tclVar("<No active dataset>  "), envir=.GlobalEnv)
    assign(".dataSetLabel", tklabel(controlsFrame, textvariable=.dataSetName, fg="red"),
        envir=.GlobalEnv)
    tkgrid(tklabel(controlsFrame, bitmap=paste("@", .images, "Rcmdr.xbm", sep=""), fg="red"), 
        tklabel(controlsFrame, text="Data set:"), .dataSetLabel, editButton, viewButton, 
        tklabel(controlsFrame, text="  Log commands:"), logCheckBox, sticky="w")
    tkgrid(controlsFrame, sticky="w", columnspan="2")
    logFrame <- tkframe(.commander)
    logXscroll <- tkscrollbar(logFrame, repeatinterval=5, orient="horizontal",
        command=function(...) tkxview(.log, ...))
    logYscroll <- tkscrollbar(logFrame, repeatinterval=5,
        command=function(...) tkyview(.log, ...))
    assign(".log", tktext(logFrame, bg="white", font=.logFont, 
        height="10", width="50",
        xscrollcommand=function(...) tkset(logXscroll, ...),
        yscrollcommand=function(...) tkset(logYscroll, ...),
        wrap="none"),  envir=.GlobalEnv)
    tkgrid(.log, logYscroll)
    tkgrid(logXscroll)
    tkgrid.configure(logYscroll, sticky="ns")
    tkgrid.configure(logXscroll, sticky="ew")
    tkgrid(logFrame, columnspan="2")
    assign(".modelName", tclVar("<No active model>"), envir=.GlobalEnv)
    bottomLeftFrame <- tkframe(.commander)
    assign(".modelLabel", tklabel(bottomLeftFrame, textvariable=.modelName, fg="red"), envir=.GlobalEnv)
    tkgrid(tklabel(bottomLeftFrame, text="Model: "), .modelLabel)
    tkgrid(bottomLeftFrame, submitButton)
    tkgrid.configure(submitButton, sticky="e")
    tkgrid.configure(bottomLeftFrame, sticky="w")
    tkfocus(.commander)
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
    result <- try(eval(parse(text=logger(command))), silent=TRUE)
    if (class(result)[1] ==  "try-error"){
        tkmessageBox(message=paste("Error:",
            strsplit(result, ":")[[1]][2]), icon="error")
        return()
        }
    print(result)
    }

justDoIt <- function(command) {
    result <- try(eval(parse(text=command)), silent=TRUE)
    if (class(result)[1] ==  "try-error"){
        tkmessageBox(message=paste("Error:",
            strsplit(result, ":")[[1]][2]), icon="error")
        return()
        }
    result
    }
    
