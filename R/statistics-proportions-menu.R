# Statistics Menu dialogs

# last modified 11 June 03 by J. Fox

    # Proportions menu
    
singleProportionTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Single-Sample Proportion Test")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .twoLevelFactors) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .twoLevelFactors[as.numeric(tkcurselection(xBox)) + 1]
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        test <- as.character(tclvalue(testVariable))
        p <- tclvalue(pVariable)
        tkgrab.release(top)
        tkdestroy(top)
        command <- paste("xtabs(~", x, ", data=", .activeDataSet, ")")
        logger(paste(".Table <-", command))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table")
        if (test == "normal") doItAndPrint(paste("prop.test(rbind(.Table), alternative='", 
            alternative, "', p=", p, ", conf.level=", level, ", correct=FALSE)", sep=""))
        else if (test == "corrected") doItAndPrint(paste("prop.test(rbind(.Table), alternative='", 
            alternative, "', p=", p, ", conf.level=", level, ", correct=TRUE)", sep=""))
        else doItAndPrint(paste("binom.test(rbind(.Table), alternative='", 
            alternative, "', p=", p, ", conf.level=", level, ")", sep=""))
        tkfocus(.commander)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }  
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(prop.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    rightFrame <- tkframe(top)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    pFrame <- tkframe(rightFrame)
    pVariable <- tclVar(".5")
    pField <- tkentry(pFrame, width="6", textvariable=pVariable)
    testFrame <- tkframe(top)
    testVariable <- tclVar("normal")
    normalButton <- tkradiobutton(testFrame, variable=testVariable, value="normal")
    correctedButton <- tkradiobutton(testFrame, variable=testVariable, value="corrected")
    exactButton <- tkradiobutton(testFrame, variable=testVariable, value="exact")    
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(xFrame, sticky="nw")    
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population proportion = p0"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population proportion < p0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population proportion > p0"), greaterButton, sticky="w")
    tkgrid(tklabel(pFrame, text="Null hypothesis: p = "), pField, sticky="w")
    tkgrid(pFrame, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level: "), confidenceField, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(alternativeFrame, rightFrame, sticky="nw")
    tkgrid(tklabel(testFrame, text="Type of Test"), columnspan=2, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation"), normalButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation with\ncontinuity correction", justify="left"), 
        correctedButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Exact binomial"), exactButton, sticky="w")
    tkgrid(testFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(confidenceField, sticky="e")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }

twoSampleProportionsTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Two-Sample Proportions Test")
    xFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .twoLevelFactors) tkinsert(xBox, "end", x)
    groupsFrame <- tkframe(top)
    groupsBox <- tklistbox(groupsFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupsScroll <- tkscrollbar(groupsFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(groupsBox, yscrollcommand=function(...) tkset(groupsScroll, ...))
    for (group in .twoLevelFactors) tkinsert(groupsBox, "end", group)
    onOK <- function(){
        x <- .twoLevelFactors[as.numeric(tkcurselection(xBox)) + 1]
        groups <- .twoLevelFactors[as.numeric(tkcurselection(groupsBox)) + 1]
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        test <- as.character(tclvalue(testVariable))
        if (x == groups) {
            tkmessageBox(message="Groups and response variables must be different.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            twoSampleProportionsTest()
            return()
            }

        tkgrab.release(top)
        tkdestroy(top)
        command <- paste("xtabs(~", groups, "+", x, ", data=", .activeDataSet, ")", sep="")
        logger(paste(".Table <-", command))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table")
        if (test == "normal") doItAndPrint(paste("prop.test(.Table, alternative='", 
            alternative, "', conf.level=", level, ", correct=FALSE)", sep=""))
        else doItAndPrint(paste("prop.test(.Table, alternative='", 
            alternative, "', conf.level=", level, ", correct=TRUE)", sep=""))
        logger("remove(.Table)")
        remove(.Table, envir=.GlobalEnv)
        tkfocus(.commander)
        }
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(prop.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    rightFrame <- tkframe(top)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    testFrame <- tkframe(top)
    testVariable <- tclVar("normal")
    normalButton <- tkradiobutton(testFrame, variable=testVariable, value="normal")
    correctedButton <- tkradiobutton(testFrame, variable=testVariable, value="corrected")
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(groupsBox, groupsScroll, sticky="nw")
    tkgrid(groupsFrame, xFrame, sticky="nw")    
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level: "), confidenceField, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(alternativeFrame, rightFrame, sticky="nw")
    tkgrid(tklabel(testFrame, text="Type of Test"), columnspan=2, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation"), normalButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation with\ncontinuity correction", justify="left"), 
        correctedButton, sticky="w")
    tkgrid(testFrame, sticky="w")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(groupsScroll, sticky="ns")
    tkgrid.configure(confidenceField, sticky="e")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:4) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkselection.set(groupsBox, 0)
    tkbind(top, "<Return>", onOK)
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
