# Statistics Menu dialogs

# last modified 10 July 04 by J. Fox

    # Nonparametric tests menu
    
twoSampleWilcoxonTest <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkNumeric()) return()
    if (!checkTwoLevelFactors()) return()
    initializeDialog(title="Two-Sample Wilcoxon Test")
    groupBox <- variableListBox(top, .twoLevelFactors, title="Groups (pick one)")
    responseBox <- variableListBox(top, .numeric, title="Response Variable (pick one)")
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=twoSampleWilcoxonTest, message="You must select a groups variable.")
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=twoSampleWilcoxonTest, message="You must select a response variable.")
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable))
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste(.activeDataSet,"$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
        if (test == "default"){
            doItAndPrint(paste("wilcox.test(", response, " ~ ", group, ', alternative="', 
            alternative, '", data=', .activeDataSet, ")", sep=""))
            }
        else doItAndPrint(paste("wilcox.test(", response, " ~ ", group, ", alternative='", 
            alternative, "', exact=", test=="exact", 
            ", correct=", test=="correct",", data=", .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="wilcox.test")
    radioButtons(name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=c("Two-sided", "Difference < 0", "Difference > 0"), title="Alternative Hypothesis")
    radioButtons(name="test", buttons=c("default", "exact", "normal", "correct"), 
        labels=c("Default", "Exact", "Normal approximation", "Normal approximation with\ncontinuity correction"), 
        title="Type of Test")
    tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    groupsLabel(groupsBox=groupBox, columnspan=2)
    tkgrid(alternativeFrame, testFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
    }    

pairedWilcoxonTest <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkNumeric(2)) return()
    initializeDialog(title="Paired Wilcoxon Test")
    xBox <- variableListBox(top, .numeric, title="First variable (pick one)")
    yBox <- variableListBox(top, .numeric, title="Second variable (pick one)")
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable))
        if (length(x) == 0 | length(y) == 0) {
            errorCondition(recall=pairedWilcoxonTest, message="You must select two variables.")
            return()
            }
        if (x == y) {
            errorCondition(recall=pairedWilcoxonTest, message="The two variables must be different.")
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("median(", .activeDataSet, "$", x, " - ", .activeDataSet, "$", y, 
            ", na.rm=TRUE) # median difference", sep=""))
        if (test == "default"){
             doItAndPrint(paste("wilcox.test(", .activeDataSet, "$", x, ", ", 
                .activeDataSet, "$", y,
                ", alternative='", alternative,
                "', paired=TRUE)", sep=""))           
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
    OKCancelHelp(helpSubject="wilcox.test")
    radioButtons(name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=c("Two-sided", "Difference < 0", "Difference > 0"), title="Alternative Hypothesis")
    radioButtons(name="test", buttons=c("default", "exact", "normal", "correct"), 
        labels=c("Default", "Exact", "Normal approximation", "Normal approximation with\ncontinuity correction"), 
        title="Type of Test")
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")    
    tkgrid(alternativeFrame, testFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=3, columns=2)
    }
    
KruskalWallisTest <- function(){
    if (!checkActiveDataSet()) return()
    if (!checkNumeric()) return()
    if (!checkFactors()) return()
    initializeDialog(title="Kruskal-Wallis Rank Sum Test")
    groupBox <- variableListBox(top, .factors, title="Groups (pick one)")
    responseBox <- variableListBox(top, .numeric, title="Response Variable (pick one)")
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=KruskalWallisTest, message="You must select a groups variable.")
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=KruskalWallisTest, message="You must select a response variable.")
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", response, sep=""),
            ", ", paste(.activeDataSet, "$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("kruskal.test(", response, " ~ ", group, ", data=",
            .activeDataSet, ")", sep=""))
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="kruskal.test")
    tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=2, columns=2)
    }
