# Statistics Menu dialogs

# last modified 10 July 04 by J. Fox

    # Means menu

independentSamplesTTest <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkNumeric()) return()
    if (!checkTwoLevelFactors()) return()
    initializeDialog(title="Independent Samples t-Test")
    variablesFrame <- tkframe(top)
    groupBox <- variableListBox(variablesFrame, .twoLevelFactors, title="Groups (pick one)")
    responseBox <- variableListBox(variablesFrame, .numeric, title="Response Variable (pick one)")
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=independentSamplesTTest, message="You must select a groups variable.")
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=independentSamplesTTest, message="You must select a response variable.")
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        variances <- as.character(tclvalue(variancesVariable))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("t.test(", response, "~", group,
            ", alternative='", alternative, "', conf.level=", level,
            ", var.equal=", variances,
            ", data=", .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="t.test")
    optionsFrame <- tkframe(top)
    radioButtons(optionsFrame, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=c("Two-sided", "Difference < 0", "Difference > 0"), title="Alternative Hypothesis")
    confidenceFrame <- tkframe(optionsFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    radioButtons(optionsFrame, name="variances", buttons=c("yes", "no"), values=c("TRUE", "FALSE"), initialValue="FALSE",
        labels=c("Yes", "No"), title="Assume equal variances?")
    tkgrid(getFrame(groupBox), tklabel(variablesFrame, text="    "), getFrame(responseBox), sticky="nw")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level", fg="blue"),sticky="w")
    tkgrid(confidenceField, sticky="w")
    groupsLabel(groupsBox=groupBox)
    tkgrid(alternativeFrame, tklabel(optionsFrame, text="    "), confidenceFrame, tklabel(optionsFrame, text="    "),
        variancesFrame, sticky="nw")
    tkgrid(optionsFrame, sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

pairedTTest <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkNumeric(2)) return()
    initializeDialog(title="Paired t-Test")
    xBox <- variableListBox(top, .numeric, title="First variable (pick one)")
    yBox <- variableListBox(top, .numeric, title="Second variable (pick one)")
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if (length(x) == 0 | length(y) == 0){
            errorCondition(recall=pairedTTest, message="You must select two variables.")
            return()
            }
        if (x == y){
            errorCondition(recall=pairedTTest, message="Variables must be different.")
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        tkdestroy(top)
        doItAndPrint(paste("t.test(", .activeDataSet, "$", x, ", ", 
            .activeDataSet, "$", y,
            ", alternative='", alternative, "', conf.level=", level, 
            ", paired=TRUE)", sep=""))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="t.test")
    radioButtons(top, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=c("Two-sided", "Difference < 0", "Difference > 0"), title="Alternative Hypothesis")
    confidenceFrame <- tkframe(top)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")    
    tkgrid(tklabel(confidenceFrame, text="Confidence Level", fg="blue"))
    tkgrid(confidenceField, sticky="w")
    tkgrid(alternativeFrame, confidenceFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=3, columns=2)
    }

singleSampleTTest <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkNumeric()) return()
    initializeDialog(title="Single-Sample t-Test")
    xBox <- variableListBox(top, .numeric, title="Variable (pick one)")
    onOK <- function(){
        x <- getSelection(xBox)
        if (length(x) == 0){
            errorCondition(recall=singleSampleTTest, message="You must select a variable.")
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        mu <- tclvalue(muVariable)
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("t.test(", .activeDataSet, "$", x,
            ", alternative='", alternative, "', mu=", mu, ", conf.level=", level, 
            ")", sep=""))
        tkdestroy(top)
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="t.test")
    radioButtons(top, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=c("Population mean = mu0", "Population mean < mu0", text="Population mean > mu0"), 
        title="Alternative Hypothesis")
    rightFrame <- tkframe(top)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    muFrame <- tkframe(rightFrame)
    muVariable <- tclVar("0.0")
    muField <- tkentry(muFrame, width="8", textvariable=muVariable)
    tkgrid(getFrame(xBox), sticky="nw") 
    tkgrid(tklabel(rightFrame, text=""), sticky="w")   
    tkgrid(tklabel(muFrame, text="Null hypothesis: mu = "), muField, sticky="w")
    tkgrid(muFrame, sticky="w")
    tkgrid(tklabel(confidenceFrame, text="Confidence Level: "), confidenceField, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(alternativeFrame, rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(confidenceField, sticky="e")
    dialogSuffix(rows=4, columns=2)
    }

oneWayAnova <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkNumeric()) return()
    if (!checkFactors()) return()
    initializeDialog(title="One-Way Analysis of Variance")
    groupBox <- variableListBox(top, .factors, title="Groups (pick one)")
    responseBox <- variableListBox(top, .numeric, title="Response Variable (pick one)")
    optionsFrame <- tkframe(top)
    pairwiseVariable <- tclVar("0")
    pairwiseCheckBox <- tkcheckbutton(optionsFrame, variable=pairwiseVariable)
    onOK <- function(){
        group <- getSelection(groupBox)
        response <- getSelection(responseBox)
        if (length(group) == 0){
            errorCondition(recall=oneWayAnova, message="You must selection a groups factor.")
            return()
            }
        if (length(response) == 0){
            errorCondition(recall=oneWayAnova, message="You must selection a response variable.")
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("anova(lm(", response, " ~ ", group, ", data=", .activeDataSet, "))", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", ", .activeDataSet, "$", group, 
            ", mean, na.rm=TRUE) # means", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", ", .activeDataSet, "$", group, 
            ", sd, na.rm=TRUE) # std. deviations", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", ", .activeDataSet, "$", group, 
            ", function(x) sum(!is.na(x))) # counts", sep=""))
        pairwise <- tclvalue(pairwiseVariable)
        if (pairwise == 1) {
            if (eval(parse(text=paste("length(levels(", .activeDataSet, "$", group, ")) < 3")))) 
                tkmessageBox (message="Factor has fewer than 3 levels; pairwise comparisons omitted.",
                    icon="warning", type="ok")
            else doItAndPrint(paste("summary(simtest(", response, " ~ ", group, 
                ', type="Tukey", data=', .activeDataSet, '))', sep=""))
            }
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="anova")
    tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    tkgrid(tklabel(optionsFrame, text="Pairwise comparisons of means"), pairwiseCheckBox, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
    }
    
multiWayAnova <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkNumeric()) return()
    if (!checkFactors()) return()
    initializeDialog(title="Multi-Way Analysis of Variance")
    groupBox <- variableListBox(top, .factors, selectmode="multiple", title="Factors (pick one or more)")
    responseBox <- variableListBox(top, .numeric, title="Response Variable (pick one)")
    onOK <- function(){
        groups <- getSelection(groupBox)
        response <- getSelection(responseBox)
        if (length(groups) == 0){
            errorCondition(recall=multiWayAnova, message="You must selection at least one factor.")
            return()
            }
        if (length(response) == 0){
            errorCondition(recall=multiWayAnova, message="You must selection a response variable.")
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        groups.list <- paste(paste(groups, "=", .activeDataSet, "$", groups, sep=""), collapse=", ")
        doItAndPrint(paste("Anova(lm(", response, " ~ ", paste(groups, collapse="*"),
             ", data=", .activeDataSet, "))", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
             "), mean, na.rm=TRUE) # means", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
             "), sd, na.rm=TRUE) # std. deviations", sep=""))
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
             "), function(x) sum(!is.na(x))) # counts", sep=""))
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="Anova")
    tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=3, columns=2)
    }
