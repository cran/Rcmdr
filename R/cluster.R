# this code by Dan Putler, used with permission

assignCluster <- function(clusterData, origData, clusterVec){
    rowsDX <- row.names(clusterData)
    rowsX <- row.names(origData)
    clustAssign <- rep(NA, length(rowsX))
    validData <- rowsX %in% rowsDX
    clustAssign[validData] <- clusterVec
    return(as.factor(clustAssign))
    }

KMeans <- function (x, centers, iter.max=10, num.seeds=10) {
    KM <- kmeans(x=x, centers=centers, iter.max=iter.max)
    for(i in 2:num.seeds) {
        newKM <- kmeans(x=x, centers=centers, iter.max=iter.max)
        if(sum(newKM$withinss) < sum(KM$withinss)) {
            KM <- newKM
            }
        }
    KM$tot.withinss <- sum(KM$withinss)
    xmean <- apply(x, 2, mean)
    centers <- rbind(KM$centers, xmean)
    bss1 <- as.matrix(dist(centers)^2)
    KM$betweenss <- sum(as.vector(bss1[nrow(bss1),])*c(KM$size,0))
    return(KM)
    }

listKmeansSolutions <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    if (length(objects) == 0) NULL
    else objects[sapply(objects, 
        function(.x) {
            if(mode(eval(parse(text=.x), envir=envir)) != "list" )
                return(FALSE)
            else {"cluster" == (names(eval(parse(text=.x),
                envir=envir))[1]) &
              "centers" == (names(eval(parse(text=.x), envir=envir))[2])}
            }
         )]
    }

kmeansClustering <- function(){
    if(!checkActiveDataSet()) return()
    if(!checkNumeric()) return()
    initializeDialog(title="KMeans Clustering")
    dataFrame <- tkframe(top)
    xBox <- variableListBox(dataFrame, .numeric, selectmode="multiple",
      title="Variables (pick one or more)")
    subsetBox(dataFrame)
    optionsFrame <- tkframe(top)
    clusterNumber <- tclVar("2")
    clusterNumSlider <- tkscale(optionsFrame, from=2, to=10, showvalue=TRUE,
      variable=clusterNumber, resolution=1, orient="horizontal")
    seedNumber <- tclVar("10")
    seedNumSlider <- tkscale(optionsFrame, from=1, to=20, showvalue=TRUE,
      variable=seedNumber, resolution=1, orient="horizontal")
    iterNumber <- tclVar("10")
    iterNumSlider <- tkscale(optionsFrame, from=5, to=30, showvalue=TRUE,
      variable=iterNumber, resolution=5, orient="horizontal")
    summaryClusters <- tclVar("1")
    summaryCB <- tkcheckbutton(optionsFrame)
    tkconfigure(summaryCB, variable=summaryClusters)
    plotClusters <- tclVar("1")
    plotCB <- tkcheckbutton(optionsFrame)
    tkconfigure(plotCB, variable=plotClusters)
    assignClusters <- tclVar("0")
    assignCB <- tkcheckbutton(optionsFrame)
    tkconfigure(assignCB, variable=assignClusters)
    assignName <- tclVar("KMeans")
    assignField <- tkentry(optionsFrame, width="15",
      textvariable=assignName)
    onOK <- function(){
        x <- getSelection(xBox)
        nvar <- length(x)
        subset <- trim.blanks(tclvalue(subsetVariable))
        nClusters <- tclvalue(clusterNumber)
        seeds <- tclvalue(seedNumber)
        iters <- tclvalue(iterNumber)
        clusterSummary <- tclvalue(summaryClusters)
        clusterPlot <- tclvalue(plotClusters)
        clusterAssign <- tclvalue(assignClusters)
        clusterVariable <- trim.blanks(tclvalue(assignName))
        if (clusterAssign == "1"){
           if (is.element(clusterVariable, .variables)) {
                if ("no" == tclvalue(checkReplace(clusterVariable))){
                    if (.grab.focus) tkgrab.release(top)
                    tkdestroy(top)
                    kmeansClustering()
                    return()
                    }
                }
           } 
        if (length(x)==0) {
            errorCondition(recall=kmeansClustering, 
              message="No variables selected.")
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        varFormula <- paste(x, collapse=" + ")
        vars <- paste(x, collapse=",", sep="")
        dset <- if (trim.blanks(subset) == "<all valid cases>") .activeDataSet
          else {paste(.activeDataSet, "[", .activeDataSet, "$", subset, ", ]",
            sep="")}
        xmat <- paste("model.matrix(~-1 + ", varFormula, ", ", dset, ")",
          sep="")
        command <- paste("KMeans(", xmat, ", centers = ", nClusters,
          ", iter.max = ", iters, ", num.seeds = ", seeds, ")", sep="")
        assign(".cluster", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".cluster <- ", command, sep=""))
        if (clusterSummary == "1") {
            doItAndPrint(paste(".cluster$size # Cluster Sizes"))
            doItAndPrint(paste(".cluster$centers # Cluster Centroids"))
            doItAndPrint(paste(
              ".cluster$withinss # Within Cluster Sum of Squares"))
            doItAndPrint(paste(
              ".cluster$tot.withinss # Total Within Sum of Squares"))
            doItAndPrint(paste(
              ".cluster$betweenss # Between Cluster Sum of Squares"))
            }
        if (clusterPlot == "1") {
            plotCommand <- paste("biplot(princomp(", xmat, 
              "), xlabs = as.character(.cluster$cluster))", sep="")
           justDoIt(plotCommand)
           logger(plotCommand)
           }
        if (clusterAssign == "1") {
            assignCommand <- paste(.activeDataSet, "$", clusterVariable,
              " <- assignCluster(", xmat, ", ", .activeDataSet,
              ", .cluster$cluster)", sep="")
            justDoIt(assignCommand)
            logger(assignCommand)
            activeDataSet(.activeDataSet)
            }
        justDoIt(paste("remove(.cluster)"))
        logger(paste("remove(.cluster)"))
        tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="KMeans")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Number of clusters:"),
      clusterNumSlider, sticky="sw")
    tkgrid(tklabel(optionsFrame, text="Number of starting seeds:"),
      seedNumSlider, sticky="sw")
    tkgrid(tklabel(optionsFrame, text="Maximum iterations:"),
      iterNumSlider, sticky="sw")
    tkgrid(tklabel(optionsFrame, 
      text="Print cluster summary"), summaryCB, sticky="w")
    tkgrid(tklabel(optionsFrame, 
      text="Bi-plot of clusters"), plotCB, sticky="w")
    tkgrid(tklabel(optionsFrame, 
      text="Assign clusters to\nthe data set         "),
      assignCB, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Assignment variable: "),
      assignField, sticky="w")
    tkgrid(dataFrame, tklabel(top, text="  "), optionsFrame,
        sticky="nw")
    tkgrid(buttonsFrame, columnspan=3, sticky="w")
    dialogSuffix(rows=3, columns=3)
    }

listHclustSolutions <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    if (length(objects) == 0) NULL
    else objects[sapply(objects, 
        function(.x) "hclust" == (class(eval(parse(text=.x), envir=envir))[1]))]
    }

hierarchicalCluster <- function(){
    if(!checkActiveDataSet()) return()
    if(!checkNumeric()) return()
    solutionNumber=length(listHclustSolutions())
    initializeDialog(title="Hierarchical Clustering")
    solutionFrame <- tkframe(top)
    solutionName <- tclVar(paste("HClust.", (solutionNumber+1),
        sep=""))
    solutionField <- tkentry(solutionFrame, width="20",
      textvariable=solutionName)
    dataFrame <- tkframe(top)
    xBox <- variableListBox(dataFrame, .numeric, selectmode="multiple",
      title="Variables (pick one or more)")
    subsetBox(dataFrame)
    radioButtons(name="method",
      buttons=c("ward", "single", "complete","average", "mcquitty", "median",
      "centroid"), labels=c("Ward's Method", "Single Linkage",
      "Complete Linkage", "Average Linkage", "McQuitty's Method",
      "Median Linkage", "Centroid Linkage"), title="Clustering Method")
    optionsFrame <- tkframe(top)
    radioButtons(optionsFrame, name="distanceType", buttons=c("euc", "euc2",
      "city", "none"), labels=c("Euclidean", "Squared-Euclidian", 
      "Manhattan (City Block)", "No Transformation"), title="Distance Measure")
    checkFrame <- tkframe(optionsFrame)
    plotDendro <- tclVar("1")
    plotCB <- tkcheckbutton(checkFrame)
    tkconfigure(plotCB, variable=plotDendro)
    onOK <- function(){
        x <- getSelection(xBox)
        nvar <- length(x)
        clusMethod <- tclvalue(methodVariable)
        distance <- tclvalue(distanceTypeVariable)
        subset <- trim.blanks(tclvalue(subsetVariable))
        dendro <- tclvalue(plotDendro)
        solution <- trim.blanks(tclvalue(solutionName))
        if (length(x)==0) {
            errorCondition(recall=hierarchicalCluster, 
              message="No variables selected.")
            return()
            }
        if (.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        varFormula <- paste(x, collapse="+")
        vars <- paste(x, collapse=",", sep="")
        dset <- if (subset == "<all valid cases>") .activeDataSet
          else {paste(.activeDataSet, "[", .activeDataSet, "$", subset, ", ]",
            sep="")}
        xmat <- paste("model.matrix(~-1 + ", varFormula, ", ", dset, ")",
          sep="")
        if(distance=="euc") {
            dx <- paste("dist(", xmat, ")", sep="")
            distlab <- "euclidian"
        }
        else if(distance=="euc2") {
            dx <- paste("dist(", xmat, ")^2", sep="")
            distlab <- "squared-euclidian"
        }
        else if(distance=="city") {
            dx <- paste("dist(", xmat, ", method= ", '"manhattan"', ")",
                sep="")
            distlab <- "city-block"
        }
        else {
            dx <- xmat
            distlab <- "untransformed"
        }
        command <- paste("hclust(", dx, " , method= ", '"', clusMethod, '"',
          ")", sep="")
        assign(solution, justDoIt(command), envir=.GlobalEnv)
        logger(paste(solution, " <- ", command, sep=""))
        if (dendro == "1") {
            justDoIt(paste("plot(", solution, ", main= ",'"',
              "Cluster Dendrogram for Solution ", solution, '"', ", xlab= ",
              '"',"Observation Number in Data Set ", dset, '"',
               ", sub=", '"', "Method=", clusMethod,
              "; Distance=", distlab, '"', ")", sep=""))
            logger(paste("plot(", solution, ", main= ",'"',
              "Cluster Dendrogram for Solution ", solution, '"', ", xlab= ",
              '"',"Observation Number in Data Set ", dset, '"',
               ", sub=", '"', "Method=", clusMethod,
              "; Distance=", distlab, '"', ")",
              sep=""))
            }
         tkfocus(.commander)
        }
    OKCancelHelp(helpSubject="hclust")
    tkgrid(solutionField, sticky="w")
    tkgrid(tklabel(top, text="Clustering solution name:"),
      solutionFrame, sticky="w")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(distanceTypeFrame, sticky="w")
    tkgrid(tklabel(checkFrame, text="  "), sticky="w")
    tkgrid(tklabel(checkFrame, text="Plot Dendrogram  "), plotCB,
      sticky="w")
    tkgrid(checkFrame, sticky="w")
    tkgrid(dataFrame, methodFrame, optionsFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=3, sticky="w")
    dialogSuffix(rows=3, columns=3)
    }

hclustSummary <- function(){
    if(!checkActiveDataSet()) return()
    parseDataSet <- function(x) {
        y <- eval(parse(text=paste(x, "$call", sep="")))
        string1 <- unlist(strsplit(as.character(y)[2], "\\("))
        string2 <- unlist(strsplit(string1[3], ","))
        if(length(grep("\\[", string2[2])) == 0) {
            out <- gsub(")", "", gsub(" ", "", gsub("\\^2", "",
                string2[2])))
            }
        else {
            string3 <- unlist(strsplit(string2[2], "\\["))
            out <- gsub(" ", "", string3[1])
            }
        return(out)
        }
    hclustObjects <- listHclustSolutions()
    testDataSet <- tapply(hclustObjects, as.factor(1:length(hclustObjects)),
      parseDataSet)
    validHclust <- hclustObjects[testDataSet==.activeDataSet]
    initializeDialog(
      title="Hierarchical Cluster Summary")
    hclustBox <- variableListBox(top, validHclust, selectmode="single",
      title="Select One Clustering Solution")
    optionsFrame <- tkframe(top)
    clusterNumber <- tclVar("2")
    slider <- tkscale(optionsFrame, from=2, to=10, showvalue=TRUE,
      variable=clusterNumber, resolution=1, orient="horizontal")
    summaryClusters <- tclVar("1")
    summaryCB <- tkcheckbutton(optionsFrame)
    tkconfigure(summaryCB, variable=summaryClusters)
    plotClusters <- tclVar("1")
    plotCB <- tkcheckbutton(optionsFrame)
    tkconfigure(plotCB, variable=plotClusters)
    if(length(hclustObjects)==0) {
        errorCondition(recall=return,
          message="There are no hierachical clustering solutions")
        }
    if(length(validHclust)==0) {
        errorCondition(recall=return, message=
     "No hierachical clustering solutions are associated with this data set.")
        }
   onOK <- function(){
        solution <- getSelection(hclustBox)
        if(length(solution)==0) {
          errorCondition(recall=appendHclustGroup,
            message="A clustering solution has not been selected.")
          return()
            }
        clusters <- as.numeric(tclvalue(clusterNumber))
        clusterVar <- paste("cutree(", solution, ", k = ", clusters, ")",
          sep="")
        clusterSummary <- tclvalue(summaryClusters)
        clusterPlot <- tclvalue(plotClusters)
        hclustCall <- eval(parse(text=paste(solution,"$call",sep="")))
        string1 <- unlist(strsplit(as.character(hclustCall)[2], "\\("))
        string2 <- unlist(strsplit(string1[3], ","))
        form.vars <- string2[1]
        if(length(grep("\\[", string2[2])) == 0) {
            xmat <- paste("model.matrix(", form.vars, ", ", .activeDataSet, ")",
              sep="")
            }
        else {
            string3 <- unlist(strsplit(string2[2], "\\["))
            xmat <- paste("model.matrix(", form.vars, ", ", .activeDataSet, "[",
              string3[2], ", ]",")", sep="")
            }
        if (clusterSummary == "1") {
            doItAndPrint(paste("summary(as.factor(", clusterVar,
              ")) # Cluster Sizes", sep=""))
            centroidsCommand <- paste("by(", xmat, ", as.factor(", clusterVar,
              "), mean) # Cluster Centroids", sep="")
            doItAndPrint(centroidsCommand)
            }
        if (clusterPlot == "1") {
             plotCommand <- paste("biplot(princomp(", xmat, 
               "), xlabs = as.character(", clusterVar, "))", sep="")
            justDoIt(plotCommand)
            logger(plotCommand)
            }
        if(.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        } 
    OKCancelHelp(helpSubject="biplot")
    tkgrid(tklabel(optionsFrame, text="Number of clusters:"), slider,
      sticky="sw")
    tkgrid(tklabel(optionsFrame, 
      text="Print cluster summary"), summaryCB, sticky="w")
    tkgrid(tklabel(optionsFrame, 
      text="Bi-plot of clusters"), plotCB, sticky="w")
    tkgrid(getFrame(hclustBox), optionsFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=2, columns=3)
    }

appendHclustGroup <- function(){
    if(!checkActiveDataSet()) return()
    parseDataSet <- function(x) {
        y <- eval(parse(text=paste(x, "$call", sep="")))
        string1 <- unlist(strsplit(as.character(y)[2], "\\("))
        string2 <- unlist(strsplit(string1[3], ","))
        if(length(grep("\\[", string2[2])) == 0) {
            out <- gsub(")", "", gsub(" ", "", gsub("\\^2", "",
                string2[2])))
            }
        else {
            string3 <- unlist(strsplit(string2[2], "\\["))
            out <- gsub(" ", "", string3[1])
            }
        return(out)
        }
    hclustObjects <- listHclustSolutions()
    if(length(hclustObjects)==0) {
        tkmessageBox(message="There are no hierachical clustering solutions", 
            icon = "error", type = "ok", default = "ok")
        return()
        }    
    testDataSet <- tapply(hclustObjects, as.factor(1:length(hclustObjects)),
      parseDataSet)
    validHclust <- hclustObjects[testDataSet==.activeDataSet]
    if(length(validHclust)==0) {
        tkmessageBox(message="No hierachical clustering solutions are associated with this data set.", 
            icon = "error", type = "ok", default = "ok")
        return()
        }
    initializeDialog(
      title="Append Cluster Groups to the Active Data Set")
    hclustBox <- variableListBox(top, validHclust, selectmode="single",
      title="Select One Clustering Solution")
    optionsFrame <- tkframe(top)
    labelName <- tclVar("hclus.label")
    labelNameField <- tkentry(optionsFrame, width="15",
      textvariable=labelName)
    clusterNumber <- tclVar("2")
    slider <- tkscale(optionsFrame, from=2, to=10, showvalue=TRUE,
      variable=clusterNumber, resolution=1, orient="horizontal")
   onOK <- function(){
        solution <- getSelection(hclustBox)
        if(length(solution)==0) {
          errorCondition(recall=appendHclustGroup,
            message="A clustering solution has not been selected.")
          return()
            }
        clusters <- as.numeric(tclvalue(clusterNumber))
        label <- trim.blanks(tclvalue(labelName))
        if (is.element(label, .variables)) {
            if ("no" == tclvalue(checkReplace(label))){
                if (.grab.focus) tkgrab.release(top)
                tkdestroy(top)
                appendHclustGroup()
                return()
                }
            }        
        hclustCall <- eval(parse(text=paste(solution,"$call",sep="")))
        string1 <- unlist(strsplit(as.character(hclustCall)[2], "\\("))
        string2 <- unlist(strsplit(string1[3], ","))
        form.vars <- string2[1]
        if(length(grep("\\[", string2[2])) == 0) {
            xmat <- paste("model.matrix(", form.vars, ", ", .activeDataSet, ")",
              sep="")
            }
        else {
            string3 <- unlist(strsplit(string2[2], "\\["))
            xmat <- paste("model.matrix(", form.vars, ", ", .activeDataSet, "[",
              string3[2], ", ]",")", sep="")
            }
        clusterVar <- paste("cutree(", solution, ", k = ", clusters, ")",
          sep="")
        command <- paste(.activeDataSet, "$", label, " <- assignCluster(",
          xmat, ", ", .activeDataSet, ", ", clusterVar, ")", sep="")
        justDoIt(command)
        logger(command)
        activeDataSet(.activeDataSet)
        if(.grab.focus) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(.commander)
        } 
    OKCancelHelp(helpSubject="assignCluster")
    tkgrid(tklabel(optionsFrame, text="  Assigned cluster label:"),
      labelNameField, sticky="w")
    tkgrid(tklabel(optionsFrame, text="  Number of clusters:"),
        slider, sticky="sw")
    tkgrid(getFrame(hclustBox), optionsFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=2, columns=3)
    }
