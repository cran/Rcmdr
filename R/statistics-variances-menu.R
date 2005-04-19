# Statistics Menu dialogs

# last modified 13 Mar 05 by J. Fox

    # Variances menu
    
twoVariancesFTest <- function(){
##    if (!checkActiveDataSet()) return()
##    if (!checkNumeric()) return()
##    if (!checkTwoLevelFactors()) return()
    initializeDialog(title="Two Variances F-Test")
    variablesFrame <- tkframe(top)
    groupBox <- variableListBox(variablesFrame, TwoLevelFactors(), title="Groups (pick one)")
    responseBox <- variableListBox(variablesFrame, Numeric(), title="Response Variable (pick one)")
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=twoVariancesFTest, message="You must select a groups variable.")
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=twoVariancesFTest, message="You must select a response variable.")
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", ", 
            .activeDataSet, "$", group, ",  var, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("var.test(", response, " ~ ", group,
            ", alternative='", alternative, "', conf.level=", level,
            ", data=", .activeDataSet, ")", sep=""))
        tkfocus(CommanderWindow())
        tkdestroy(top)
        }
    OKCancelHelp(helpSubject="var.test")
    radioButtons(name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=c("Two-sided", "Difference < 0", "Difference > 0"), title="Alternative Hypothesis")
    confidenceFrame <- tkframe(top)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    tkgrid(getFrame(groupBox), tklabel(variablesFrame, text="    "), getFrame(responseBox), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    groupsLabel(groupsBox=groupBox)
    tkgrid(tklabel(confidenceFrame, text="Confidence Level:  ", fg="blue"), confidenceField, sticky="w")
    tkgrid(alternativeFrame, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=5, columns=1)
    }

BartlettTest <- function(){
##    if (!checkActiveDataSet()) return()
##    if (!checkNumeric()) return()
##    if (!checkFactors()) return()
    initializeDialog(title="Bartlett's Test")
    variableFrame <- tkframe(top)
    groupBox <- variableListBox(variableFrame, Factors(), title="Groups (pick one)")
    responseBox <- variableListBox(variableFrame, Numeric(), title="Response Variable (pick one)")
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=BartlettTest, message="You must select a groups variable.")
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=BartlettTest, message="You must select a response variable.")
            return()
            }
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", response, sep=""),
            ", ", paste(.activeDataSet, "$", group, sep=""), ", var, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("bartlett.test(", response, " ~ ", group, ", data=",
            .activeDataSet, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="bartlett.test")
    tkgrid(getFrame(groupBox), tklabel(variableFrame, text="    "), getFrame(responseBox), sticky="nw")
    tkgrid(variableFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }

LeveneTest <- function(){
##    if (!checkActiveDataSet()) return()
##    if (!checkNumeric()) return()
##    if (!checkFactors()) return()
    initializeDialog(title="Levene's Test")
    variableFrame <- tkframe(top)
    groupBox <- variableListBox(variableFrame, Factors(), title="Groups (pick one)")
    responseBox <- variableListBox(variableFrame, Numeric(), title="Response Variable (pick one)")
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=LeveneTest, message="You must select a groups variable.")
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=LeveneTest, message="You must select a response variable.")
            return()
            }
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", response, sep=""),
            ", ", paste(.activeDataSet, "$", group, sep=""), ", var, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("levene.test(", paste(.activeDataSet, "$", response, sep=""), 
            ", ", paste(.activeDataSet, "$", group, sep=""), ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="levene.test")
    tkgrid(getFrame(groupBox), tklabel(variableFrame, text="    "), getFrame(responseBox), sticky="nw")
    tkgrid(variableFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }
