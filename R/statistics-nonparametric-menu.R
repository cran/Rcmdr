# Statistics Menu dialogs

# last modified 11 June 03 by J. Fox

    # Nonparametric tests menu
    
twoSampleWilcoxonTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Two-Sample Wilcoxon Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .twoLevelFactors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable))
        tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste(.activeDataSet,"$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
        if (test == "default"){
            doItAndPrint(paste("wilcox.test(", response, " ~ ", group, ", alternative='", 
            alternative, "')", sep=""))
            }
        else doItAndPrint(paste("wilcox.test(", response, " ~ ", group, ", alternative='", 
            alternative, "', exact=", test=="exact", 
            ", correct=", test=="correct", ")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }  
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(wilcox.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    testFrame <- tkframe(top)
    testVariable <- tclVar("default")
    defaultButton <- tkradiobutton(testFrame, variable=testVariable, value="default")
    exactButton <- tkradiobutton(testFrame, variable=testVariable, value="exact")
    normalButton <- tkradiobutton(testFrame, variable=testVariable, value="normal")
    correctButton <- tkradiobutton(testFrame, variable=testVariable, value="correct")
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Type of Test"), columnspan=2, sticky="w")
    tkgrid(tklabel(testFrame, text="Default"), defaultButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Exact"), exactButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation"), normalButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation with\ncontinuity correction", justify="left"), 
        correctButton, sticky="w")
    tkgrid(alternativeFrame, testFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }    

pairedWilcoxonTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Paired Wilcoxon Test")
    xFrame <- tkframe(top)
    yFrame <- tkframe(top)
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    tkconfigure(xBox, yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    tkconfigure(yBox, yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        y <- as.character(tkget(yBox, "active"))
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable))
        if (x == y) {
            tkmessageBox(message="Two variables must be different.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            pairedWilcoxonTest()
            return()
            }
        tkgrab.release(top)
        tkdestroy(top)
        if (test == "default"){
             doItAndPrint(paste("wilcox.test(", .activeDataSet, "$", x, ", ", 
                .activeDataSet, "$", y,
                ", alternative='", alternative,
                "')", sep=""))           
            }
        else if (test == "exact"){
            doItAndPrint(paste("wilcox.test(", .activeDataSet, "$", x, ", ", 
                .activeDataSet, "$", y,
                ", alternative='", alternative,
                "', exact=TRUE, paired=TRUE)", sep=""))
                }
        else {
            doItAndPrint(paste("wilcox.test(", .activeDataSet, "$", x, ", ", 
                .activeDataSet, "$", y,
                ", alternative='", alternative, "', correct=", test=="correct",
                ", exact=FALSE, paired=TRUE)", sep=""))
                }
        tkdestroy(top)
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
        help(wilcox.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    testFrame <- tkframe(top)
    testVariable <- tclVar("default")
    defaultButton <- tkradiobutton(testFrame, variable=testVariable, value="default")
    exactButton <- tkradiobutton(testFrame, variable=testVariable, value="exact")
    normalButton <- tkradiobutton(testFrame, variable=testVariable, value="normal")
    correctButton <- tkradiobutton(testFrame, variable=testVariable, value="correct")
    tkgrid(tklabel(top, text="First variable (pick one)"), 
        tklabel(top, text="Second variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid(xFrame, yFrame, sticky="nw")    
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Type of Test"), columnspan=2, sticky="w")
    tkgrid(tklabel(testFrame, text="Default"), defaultButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Exact"), exactButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation"), normalButton, sticky="w")
    tkgrid(tklabel(testFrame, text="Normal approximation with\ncontinuity correction", justify="left"), 
        correctButton, sticky="w")    
    tkgrid(alternativeFrame, testFrame, sticky="n")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")    
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:3) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(xBox, 0)
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
    
KruskalWallisTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Kruskal-Wallis Rank Sum Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE")
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    tkconfigure(groupBox, yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE")
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    tkconfigure(responseBox, yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", response, sep=""),
            ", ", paste(.activeDataSet, "$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("kruskal.test(", response, " ~ ", group, ", data=",
            .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12", command=onOK, default="active")
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        }
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12", command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(kruskal.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(helpButton, sticky="e")
    for (row in 0:2) tkgrid.rowconfigure(top, row, weight=0)
    for (col in 0:1) tkgrid.columnconfigure(top, col, weight=0)
    .Tcl("update idletasks")
    tkwm.resizable(top, 0, 0)
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    tkwm.deiconify(top)
    tkgrab.set(top)
    tkfocus(top)
    tkwait.window(top)
    }
