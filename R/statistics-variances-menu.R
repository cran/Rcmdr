# Statistics Menu dialogs

# last modified 15 May 03 by J. Fox

    # Variances menu
    
twoVariancesFTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Two Variances F-Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    groupBox <- tklistbox(groupFrame, height=min(4, length(.twoLevelFactors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .twoLevelFactors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", response, ", ", group, ",  var, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("var.test(", response, " ~ ", group,
            ", alternative='", alternative, "', conf.level=", level,
            ", data=", .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        tkdestroy(top)
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
        help(var.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    alternativeFrame <- tkframe(top)
    alternativeVariable <- tclVar("two.sided")
    twosidedButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="two.sided")
    lessButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="less")
    greaterButton <- tkradiobutton(alternativeFrame, variable=alternativeVariable, value="greater")
    confidenceFrame <- tkframe(top)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid(groupFrame, responseFrame, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level"))
    tkgrid(confidenceField)
    tkgrid(alternativeFrame, confidenceFrame, sticky="n")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

BartlettTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Bartlett's Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", response, sep=""),
            ", ", paste(.activeDataSet, "$", group, sep=""), ", var, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("bartlett.test(", response, " ~ ", group, ", data=",
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
        help(bartlett.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

LeveneTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Levene's Test")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        group <- as.character(tkget(groupBox, "active"))
        response <- as.character(tkget(responseBox, "active"))
        tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", response, sep=""),
            ", ", paste(.activeDataSet, "$", group, sep=""), ", var, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("levene.test(", paste(.activeDataSet, "$", response, sep=""), 
            ", ", paste(.activeDataSet, "$", group, sep=""), ")", sep=""))
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
        help(levene.test)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }
