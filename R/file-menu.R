# last modified 23 May 2003 by J. Fox

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
    response <- tclvalue(tkmessageBox(message="Exit?",
        icon="question", type="okcancel", default="cancel"))
    if (response == "cancel") return(invisible(response))
    if (!is.null(.activeDataSet)) justDoIt(logger(paste("detach(", .activeDataSet, ")", sep="")))
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
    return(invisible(response))
    }
    
closeCommanderAndR <- function(){
    response <- closeCommander()
    if (response == "cancel") return()
    quit(save="no")
    }
