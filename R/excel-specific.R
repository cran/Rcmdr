# These functions for Excel supportwritten by Erich Neuwirth
#  last modified: 22 June 2007 by J. Fox

    SubmitToCommander <- function(myInput){
        .log <- LogWindow() 
        lines <- myInput
        lines <- strsplit(lines, "\n")[[1]]
        .console.output <- getRcmdr("console.output")
        .output <- OutputWindow()
        if (!.console.output) tkinsert(.output, "end", "\n")
        iline <- 1
        nlines <- length(lines)
        while (iline <= nlines){
            current.line <- lines[iline]
            jline <- iline + 1
            while (jline <= nlines){
                if (length(grep("^[\\ \t]", lines[jline])) == 0) break
                current.line <- paste(current.line, lines[jline])
                jline <- jline + 1
                iline <- iline + 1
                }
            if (length(grep("<-", current.line)) > 0){
                justDoIt(current.line)
                }
            else if (length(grep("^remove\\(", current.line)) > 0){
                current.line <- sub(")", ", envir=.GlobalEnv)", current.line)
                justDoIt(current.line)
                }
            else if (any(sapply(Commander.Input.exceptions,
                    function(.x) length(grep(paste("^", .x, "\\(", sep=""), current.line)) > 0))){
                justDoIt(current.line)
                }   
            else doItAndPrint(current.line)
            iline <- iline + 1
            }
	dummyarg<-tkyview.moveto(.output, 1)
        }


    RExcelSupported <- function(){
    	RExcelSupport <- getOption("Rcmdr")$RExcelSupport
        !is.null(RExcelSupport) && RExcelSupport && exists("RExcelEnv") &&
            exists("putRExcel", where="RExcelEnv")
    	}