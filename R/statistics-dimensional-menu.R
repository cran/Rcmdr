# Statistics Menu dialogs

# last modified 1 July 04 by J. Fox

    # Dimensional-analysis menu
    
Reliability <- function(){
    if(!checkActiveDataSet()) return()
    if (!checkNumeric(3)) return()
    initializeDialog(title="Scale Reliability")
    xBox <- variableListBox(top, .numeric, selectmode="multiple", title="Variables (pick three or more)")
    onOK <- function(){
        x <- getSelection(xBox)
        if (3 > length(x)) {
            errorCondition(recall=Reliability, message="Fewer than 3 variables selected.")
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        x <- paste('"', x, '"', sep="")
        doItAndPrint(paste("reliability(cov(", .activeDataSet, "[,c(", paste(x, collapse=","),
            ')], use="complete.obs"))', sep=""))
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="reliability")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }

principalComponents <- function(){
    if(!checkActiveDataSet()) return()
    if(!checkNumeric(2)) return()
    initializeDialog(title="Principal Components Analysis")
    xBox <- variableListBox(top, .numeric, selectmode="multiple", title="Variables (pick two or more)")
    subsetBox()
    checkBoxes(frame="optionsFrame", boxes=c("correlations", "screeplot", "addPC"), initialValues=c("1", "0", "0"),
        labels=c("Analyze correlation matrix", "Screeplot", "Add principal components to data set"))
    onOK <- function(){
        x <- getSelection(xBox)
        nvar <- length(x)
        correlations <- tclvalue(correlationsVariable)
        subset <- tclvalue(subsetVariable)
        screeplot <- tclvalue(screeplotVariable)
        addPC <- tclvalue(addPCVariable)
        if (2 > length(x)) {
            errorCondition(recall=principalComponents, message="Fewer than 2 variables selected.")
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" else paste(", subset=", subset, sep="")
        correlations <- if (correlations == "1") "TRUE" else "FALSE"
        command <- paste("princomp(~", paste(x, collapse="+"), ", cor=", correlations,
            ", data=", .activeDataSet, subset, ")", sep="")
        assign(".PC", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".PC <- ", command, sep=""))
        doItAndPrint("unclass(loadings(.PC))  # component loadings")
        doItAndPrint(".PC$sd^2  # component variances")
        if (screeplot == "1") {
            justDoIt("screeplot(.PC)")
            logger("screeplot(.PC)")
            }
        if (addPC == "1") {
            for(i in 1:nvar){
                var <- paste("PC", i, sep="")
                if (is.element(var, .variables)) {
                    if ("no" == tclvalue(checkReplace(var))) next
                    }
                justDoIt(paste(.activeDataSet, "$PC", i, " <- .PC$scores[,", i, "]", sep=""))
                logger(paste(.activeDataSet, "$PC", i, " <- .PC$scores[,", i, "]", sep=""))
                }
            activeDataSet(.activeDataSet)
            remove(.PC, envir=.GlobalEnv)   
            }
        logger("remove(.PC)")
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="princomp")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

factorAnalysis <- function(){
    if(!checkActiveDataSet()) return()
    if(!checkNumeric(3)) return()
    initializeDialog(title="Factor Analysis")
    xBox <- variableListBox(top, .numeric, selectmode="multiple", title="Variables (pick three or more)")
    subsetBox()
    optionsFrame <- tkframe(top)
    nfactorVariable <- tclVar("1")
    nfactorEntry <- tkentry(optionsFrame, width="2", textvariable=nfactorVariable)
    checkFrame <- tkframe(top)
    radioButtons(checkFrame, name="rotation", buttons=c("noRotate", "varimax", "promax"), 
        values=c("none", "varimax", "promax"), initialValue="varimax", labels=c("None", "Varimax", "Promax"),
        title="Factor Rotation")
    radioButtons(checkFrame, name="scores", buttons=c("noScores", "bartlett", "regression"),
        values=c("none", "Bartlett", "regression"), labels=c("None", "Bartlett's method", "Regression method"),
        title="Factor Scores")
    onOK <- function(){
        x <- getSelection(xBox)
        nvar <- length(x)
        nfactor <- as.numeric(tclvalue(nfactorVariable))
        subset <- tclvalue(subsetVariable)
        rotation <- tclvalue(rotationVariable)
        scores <- tclvalue(scoresVariable)
        if (3 > length(x)) {
            errorCondition(recall=factorAnalysis, message="Fewer than 3 variables selected.")
            return()
            }
        f <- function(k, p) ((p - k)^2 - p - k)^2
        max.factors <- floor(optimize(f, c(0, nvar), tol=.0001, p=nvar)$minimum)
        if (nfactor > max.factors) {
            errorCondition(recall=factorAnalysis, message=
                if (max.factors > 1) paste("Number of factors must be between 1 and ", max.factors, ".", sep="")
                    else "Number of factors cannot exceed 1.")
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        subset <- if (trim.blanks(subset) == "<all valid cases>") "" else paste(", subset=", subset, sep="")
        command <- paste("factanal(~", paste(x, collapse="+"), ", factors=", nfactor, ', rotation="', rotation,
            '", scores="', scores, '", data=', .activeDataSet, subset, ")", sep="")
        assign(".FA", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".FA <- ", command, sep=""))
        doItAndPrint(".FA")
        if (scores != "none") {
            for(i in 1:nfactor){
                var <- paste("F", i, sep="")
                if (is.element(var, .variables)) {
                    if ("no" == tclvalue(checkReplace(var))) next
                    }
                justDoIt(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", i, "]", sep=""))
                logger(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", i, "]", sep=""))
                }
            activeDataSet(.activeDataSet)
            }
        logger("remove(.FA)")
        remove(.FA, envir=.GlobalEnv)
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="factanal")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Number of factors:"),
        nfactorEntry, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(rotationFrame, tklabel(checkFrame, text="    "), scoresFrame, sticky="w")
    tkgrid(checkFrame, sticky="w")
    tkgrid(buttonsFrame,  sticky="w")
    dialogSuffix(rows=5, columns=1)
    }
