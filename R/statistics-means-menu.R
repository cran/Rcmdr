# Statistics Menu dialogs

# last modified 20 May 03 by J. Fox

    # Means menu

independentSamplesTTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Independent Samples t-Test")
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
        variances <- as.character(tclvalue(variancesVariable))
        tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("t.test(", response, "~", group,
            ", alternative='", alternative, "', conf.level=", level,
            ", var.equal=", variances,
            ", data=", .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        }
    buttonsFrame <- tkframe(top)
    OKbutton <- tkbutton(buttonsFrame, text="OK", width="12",command=onOK, default="active")
    onCancel <- function() {
        tkgrab.release(top)
        tkfocus(.commander)
        tkdestroy(top)  
        } 
    cancelButton <- tkbutton(buttonsFrame, text="Cancel", width="12",command=onCancel)
    onHelp <- function() {
        if (.Platform$OS.type != "windows") tkgrab.release(top)
        help(t.test)
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
    variancesFrame <- tkframe(top)
    variancesVariable <- tclVar("FALSE")
    yesButton <- tkradiobutton(variancesFrame, variable=variancesVariable, value="TRUE")
    noButton <- tkradiobutton(variancesFrame, variable=variancesVariable, value="FALSE")
    tkgrid(tklabel(top, text="Groups (pick one)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid(groupFrame, responseFrame, sticky="n")
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Two-sided"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference < 0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Difference > 0"), greaterButton, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level"))
    tkgrid(confidenceField)
    tkgrid(tklabel(variancesFrame, text="Assume equal variance?"), columnspan=2, sticky="w")
    tkgrid(tklabel(variancesFrame, text="No"), noButton, sticky="w")
    tkgrid(tklabel(variancesFrame, text="Yes"), yesButton, sticky="w")
    tkgrid(alternativeFrame, confidenceFrame, variancesFrame, sticky="n")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, tklabel(top, text=""), helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(groupBox, 0)
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

pairedTTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Paired t-Test")
    xFrame <- tkframe(top)
    yFrame <- tkframe(top)
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    yScroll <- tkscrollbar(yFrame, repeatinterval=5, command=function(...) tkyview(yBox, ...))    
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    yBox <- tklistbox(yFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(yScroll, ...))
    for (y in .numeric) tkinsert(yBox, "end", y)
    onOK <- function(){
        x <- as.character(tkget(xBox, "active"))
        y <- as.character(tkget(yBox, "active"))
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        if (x == y) {
            tkmessageBox(message="Variables must be different.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            pairedTTest()
            return()
            }
        tkdestroy(top)
        doItAndPrint(paste("t.test(", .activeDataSet, "$", x, ", ", 
            .activeDataSet, "$", y,
            ", alternative='", alternative, "', conf.level=", level, 
            ", paired=TRUE)", sep=""))
        tkgrab.release(top)
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
        help(t.test)
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
    tkgrid(tklabel(top, text="First variable (pick one)"), 
        tklabel(top, text="Second variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid(yBox, yScroll, sticky="nw")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid.configure(yScroll, sticky="ns")    
    tkgrid(xFrame, yFrame, sticky="nw")    
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
    tkselection.set(xBox, 0)
    tkselection.set(yBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

singleSampleTTest <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Single-Sample t-Test")
    xFrame <- tkframe(top)
    xScroll <- tkscrollbar(xFrame, repeatinterval=5, command=function(...) tkyview(xBox, ...))
    xBox <- tklistbox(xFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(xScroll, ...))
    for (x in .numeric) tkinsert(xBox, "end", x)
    onOK <- function(){
        x <- .numeric[as.numeric(tkcurselection(xBox)) + 1]
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        mu <- tclvalue(muVariable)
        tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("t.test(", .activeDataSet, "$", x,
            ", alternative='", alternative, "', mu=", mu, ", conf.level=", level, 
            ")", sep=""))
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
        help(t.test)
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
    muFrame <- tkframe(rightFrame)
    muVariable <- tclVar("0.0")
    muField <- tkentry(muFrame, width="8", textvariable=muVariable)
    tkgrid(tklabel(top, text="Variable (pick one)"), sticky="w")
    tkgrid(xBox, xScroll, sticky="nw")
    tkgrid.configure(xScroll, sticky="ns")
    tkgrid(xFrame, sticky="nw")    
    tkgrid(tklabel(alternativeFrame, text="Alternative Hypothesis"), columnspan=2, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population mean = mu0"), twosidedButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population mean < mu0"), lessButton, sticky="w")
    tkgrid(tklabel(alternativeFrame, text="Population mean > mu0"), greaterButton, sticky="w")
    tkgrid(tklabel(muFrame, text="Null hypothesis: mu = "), muField, sticky="w")
    tkgrid(muFrame, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level: "), confidenceField, sticky="w")
    tkgrid.configure(confidenceField, sticky="e")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(alternativeFrame, rightFrame)
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(xBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }

oneWayAnova <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "One-Way Analysis of Variance")
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
        doItAndPrint(paste("anova(lm(", response, " ~ ", group, "))", sep=""))
        doItAndPrint(paste("tapply(", response, ", ", group, ", mean) # means", sep=""))
        doItAndPrint(paste("tapply(", response, ", ", group, ", sd) # std. deviations", sep=""))
        doItAndPrint(paste("tapply(", response, ", ", group, ", length) # counts", sep=""))
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
        help(anova)
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
    
multiWayAnova <- function(){
    if (activeDataSet() == FALSE) return()
    top <- tktoplevel()
    tkwm.title(top, "Multi-Way Analysis of Variance")
    groupFrame <- tkframe(top)
    responseFrame <- tkframe(top)
    groupScroll <- tkscrollbar(groupFrame, repeatinterval=5, 
        command=function(...) tkyview(groupBox, ...))
    responseScroll <- tkscrollbar(responseFrame, repeatinterval=5, 
        command=function(...) tkyview(responseBox, ...))    
    groupBox <- tklistbox(groupFrame, height=min(4, length(.factors)),
        selectmode="multiple", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(groupScroll, ...))
    for (group in .factors) tkinsert(groupBox, "end", group)
    responseBox <- tklistbox(responseFrame, height=min(4, length(.numeric)),
        selectmode="single", background="white", exportselection="FALSE",
        yscrollcommand=function(...) tkset(responseScroll, ...))
    for (response in .numeric) tkinsert(responseBox, "end", response)
    onOK <- function(){
        groups <- .factors[as.numeric(tkcurselection(groupBox)) + 1]
        response <- as.character(tkget(responseBox, "active"))
        if (0 == length(groups)) {
            tkmessageBox(message="No factors selected.", 
                icon="error", type="ok")
            tkgrab.release(top)
            tkdestroy(top)
            multiWayAnova()
            return()
            }
        tkgrab.release(top)
        tkdestroy(top)
        groups.list <- paste(paste(groups, "=", groups, sep=""), collapse=", ")
        doItAndPrint(paste("Anova(lm(", response, " ~ ", paste(groups, collapse="*"),
             "))", sep=""))
        doItAndPrint(paste("tapply(", response, ", list(", groups.list,
             "), mean) # means", sep=""))
        doItAndPrint(paste("tapply(", response, ", list(", groups.list,
             "), sd) # std. deviations", sep=""))
        doItAndPrint(paste("tapply(", response, ", list(", groups.list,
             "), length) # counts", sep=""))
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
        help(Anova)
        }
    helpButton <- tkbutton(top, text="Help", width="12", command=onHelp)
    tkgrid(tklabel(top, text="Factors (pick one or more)"), 
        tklabel(top, text="Response Variable (pick one)"), sticky="w")
    tkgrid(groupBox, groupScroll, sticky="nw")
    tkgrid(responseBox, responseScroll, sticky="nw")
    tkgrid(groupFrame, responseFrame, sticky="nw")
    tkgrid.configure(responseScroll, sticky="ns")
    tkgrid.configure(groupScroll, sticky="ns")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, helpButton, sticky="w")
    tkgrid.configure(helpButton, sticky="e")
    tkselection.set(responseBox, 0)
    tkbind(top, "<Return>", onOK)
    tkfocus(top)
    tkgrab(top)
    }
