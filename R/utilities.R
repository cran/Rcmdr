# last modified 23 Feb 06 by J. Fox + slight changes 12 Aug 04 by Ph. Grosjean
                                                                                       
# utility functions


    # listing objects etc.

listDataSets <- function(envir=.GlobalEnv, ...) {
	Vars <- ls(envir = envir, all.names = TRUE) # + PhG
	if (length(Vars) == 0) return(Vars) # + PhG
    names(which(sapply(Vars, function(.x) is.data.frame(eval(parse(text=.x), envir=envir))))) # + PhG
    }

listLinearModels <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    if (length(objects) == 0) NULL
    else objects[sapply(objects, 
        function(.x) "lm" == (class(eval(parse(text=.x), envir=envir))[1]))]
    }

listGeneralizedLinearModels <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    if (length(objects) == 0) NULL
    else objects[sapply(objects, 
        function(.x) "glm" == (class(eval(parse(text=.x), envir=envir))[1]))]
    }

listMultinomialLogitModels <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    if (length(objects) == 0) NULL
    else objects[sapply(objects, 
        function(.x) "multinom" == (class(eval(parse(text=.x), envir=envir))[1]))]
    }

listProportionalOddsModels <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    if (length(objects) == 0) NULL
    else objects[sapply(objects, 
        function(.x) "polr" == (class(eval(parse(text=.x), envir=envir))[1]))]
    }

listAllModels <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    if (length(objects) == 0) NULL
    else objects[sapply(objects, 
        function(.x) (class(eval(parse(text=.x), envir=envir))[1])) %in% getRcmdr("modelClasses")]
    }
                
activeDataSet <- function(dsname, flushModel=TRUE){
    .activeDataSet <- ActiveDataSet()
    if (missing(dsname)) {
        if (is.null(.activeDataSet)){
            Message(message=gettextRcmdr("There is no active data set."), type="error")
            return(FALSE)
            }
        else return(.activeDataSet)
        }
    if (!is.data.frame(get(dsname, envir=.GlobalEnv))){
        Message(message=paste(dsname, gettextRcmdr(" is not a data frame and cannot be attached."),
            sep=""), type="error")
        tkfocus(CommanderWindow())
        return()
        }
    varnames <- names(eval(parse(text=dsname), envir=.GlobalEnv))
    newnames <- make.names(varnames)
    badnames <- varnames != newnames
    if (any(badnames)){
        command <- paste("names(", dsname, ") <- make.names(names(",
            dsname, "))", sep="")
        doItAndPrint(command)
        }
    if (!is.null(.activeDataSet) && getRcmdr("attach.data.set")
        && (length(grep(.activeDataSet, search())) !=0)) {
        detach(pos = match(.activeDataSet, search()))
        logger(paste("detach(", .activeDataSet, ")", sep=""))
        }
    if (flushModel) {
        putRcmdr(".activeModel", NULL)
        RcmdrTclSet("modelName", gettextRcmdr("<No active model>"))
        }
    # -PhG tkconfigure(.modelLabel, fg="red")
    ActiveDataSet(dsname)
    Message(sprintf(gettextRcmdr("The dataset %s has %d rows and %d columns."), dsname, 
        nrow(eval(parse(text=dsname))), ncol(eval(parse(text=dsname)))), type="note")
    if (any(badnames)) Message(message=paste(dsname, gettextRcmdr(" contains non-standard variable names:\n"),
        paste(varnames[badnames], collapse=", "), 
        gettextRcmdr("\nThese have been changed to:\n"), paste(newnames[badnames], collapse=", "),
        sep=""), type="warning")
    Variables(listVariables())
    Numeric(listNumeric())
    Factors(listFactors())
    TwoLevelFactors(listTwoLevelFactors())
    RcmdrTclSet("dataSetName", paste(" ", dsname, " "))
    # -PhG tkconfigure(.dataSetLabel, fg="blue")
    if (!is.SciViews()) tkconfigure(getRcmdr("dataSetLabel"), fg="blue") else refreshStatus() # +PhG
    if (getRcmdr("attach.data.set")){
        attach(get(dsname, envir=.GlobalEnv), name=dsname)
        logger(paste("attach(", dsname, ")", sep=""))
        }
    if (is.SciViews()) refreshStatus() else if (flushModel) tkconfigure(getRcmdr("modelLabel"), fg="red") # +PhG (& J.Fox, 25Dec04)
    activateMenus()
    dsname
    }


activeModel <- function(model){
    if (missing(model)) {
        .activeModel <- ActiveModel()
        if (is.null(.activeModel)){
            Message(message=gettextRcmdr("There is no active model."), type="error")
            return(FALSE)
            }
        else return(.activeModel)
        }
    ActiveModel(model)
    RcmdrTclSet("modelName", paste(" ", model, " "))
    # -PhG tkconfigure(.modelLabel, fg="blue")
    if (!is.SciViews()) tkconfigure(getRcmdr("modelLabel"), fg="blue") else refreshStatus() # +PhG
    activateMenus()
    model
    }
    
listVariables <- function(dataSet=ActiveDataSet()) {
    vars <- eval(parse(text=paste("names(", dataSet,")")), envir=.GlobalEnv)
    if (getRcmdr("sort.names")) sort(vars) else vars
    }

listFactors <- function(dataSet=ActiveDataSet()) {
    variables <- if (exists("variables", envir=RcmdrEnv())) getRcmdr("variables") else listVariables(dataSet)
    variables[sapply(variables, function(.x)
        is.factor(eval(parse(text=.x), envir=eval(parse(text=dataSet), envir=.GlobalEnv))))]
    }

listTwoLevelFactors <- function(dataSet=ActiveDataSet()){
    factors <- listFactors(dataSet)
    if(length(factors) == 0) return(NULL)
    factors[sapply(factors, function(.x)
        2 == length(levels(eval(parse(text=.x), envir=eval(parse(text=dataSet), 
            envir=.GlobalEnv)))))]
    }
    
listNumeric <- function(dataSet=ActiveDataSet()) {
    variables <- if (exists("variables", envir=RcmdrEnv())) getRcmdr("variables") else listVariables(dataSet)
    variables[sapply(variables,function(.x)
        is.numeric(eval(parse(text=.x), envir=eval(parse(text=dataSet), envir=.GlobalEnv))))]
    }

trim.blanks <- function(text){
    gsub("^\ ", "", gsub("\ *$", "", text))
    }
    
is.valid.name <- function(x){
    length(x) == 1 && is.character(x) && x == make.names(x)
    }

    
    # statistical
    
colPercents <- function(tab, digits=1){
    dim <- length(dim(tab))
    if (is.null(dimnames(tab))){
        dims <- dim(tab)
        dimnames(tab) <- lapply(1:dim, function(i) 1:dims[i])
        }
    sums <- apply(tab, 2:dim, sum)
    per <- apply(tab, 1, function(x) x/sums)
    dim(per) <- dim(tab)[c(2:dim,1)]
    per <- aperm(per, c(dim, 1:(dim-1)))
    dimnames(per) <- dimnames(tab)
    per <- round(100*per, digits)
    result <- abind(per, Total=apply(per, 2:dim, sum), Count=sums, along=1)
    names(dimnames(result)) <- names(dimnames(tab))
    result
    }

rowPercents <- function(tab, digits=1){
    dim <- length(dim(tab))
    if (dim == 2) return(t(colPercents(t(tab), digits=digits)))
    tab <- aperm(tab, c(2,1,3:dim))
    aperm(colPercents(tab, digits=digits), c(2,1,3:dim))
    }

reliability <- function(S){
    reliab <- function(S, R){
        k <- dim(S)[1]
        ones <- rep(1, k)
        v <- as.vector(ones %*% S %*% ones)
        alpha <- (k/(k - 1)) * (1 - (1/v)*sum(diag(S)))
        rbar <- mean(R[lower.tri(R)])
        std.alpha <- k*rbar/(1 + (k - 1)*rbar)
        c(alpha=alpha, std.alpha=std.alpha)
        }
    result <- list()
    if ((!is.numeric(S)) || !is.matrix(S) || (nrow(S) != ncol(S)) 
        || any(abs(S - t(S)) > max(abs(S))*1e-10) || nrow(S) < 2)
        stop(gettextRcmdr("argument must be a square, symmetric, numeric covariance matrix"))
    k <- dim(S)[1]
    s <- sqrt(diag(S))
    R <- S/(s %o% s)
    rel <- reliab(S, R)
    result$alpha <- rel[1]
    result$st.alpha <- rel[2]
    if (k < 3) {
        warning(gettextRcmdr("there are fewer than 3 items in the scale"))
        return(invisible(NULL))
        }
    rel <- matrix(0, k, 3)
    for (i in 1:k) {
        rel[i, c(1,2)] <- reliab(S[-i, -i], R[-i, -i])
        a <- rep(0, k)
        b <- rep(1, k)
        a[i] <- 1
        b[i] <- 0
        cov <- a %*% S %*% b
        var <- b %*% S %*% b
        rel[i, 3] <- cov/(sqrt(var * S[i,i]))
        }
    rownames(rel) <- rownames(S)
    colnames(rel) <- c("Alpha", "Std.Alpha", "r(item, total)")
    result$rel.matrix <- rel
    class(result) <- "reliability"
    result
    }

print.reliability <- function(x, digits=4, ...){
    cat(paste("Alpha reliability = ", round(x$alpha, digits), "\n"))
    cat(paste("Standardized alpha = ", round(x$st.alpha, digits), "\n"))
    cat("\nReliability deleting each item in turn:\n")
    print(round(x$rel.matrix, digits))
    invisible(x)
    }
    
partial.cor <- function(X, ...){
    R <- cor(X, ...)
    RI <- solve(R)
    D <- 1/sqrt(diag(RI))
    R <- - RI * (D %o% D)
    diag(R) <- 0
    rownames(R) <- colnames(R) <- colnames(X)
    R
    }

Confint <- function(object, parm, level=0.95, ...) UseMethod("Confint")

Confint.default <- function(object, parm, level = 0.95, ...) confint(object, parm, level, ...)

Confint.glm <- function (object, parm, level=0.95, type=c("LR", "Wald"), ...){
    # adapted from stats:::confint.lm
    type <- match.arg(type)
    if (type == "LR") return(MASS:::confint.glm(object, parm, level, ...))
        cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm))
        parm <- seq(along = pnames)
    else if (is.character(parm))
        parm <- match(parm, pnames, nomatch = 0)
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(round(100 * a, 1), "%")
    ci <- array(NA, dim = c(length(parm), 2), dimnames = list(pnames[parm],
        pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    fac <- qnorm(a)
    ci[] <- cf[parm] + ses %o% fac
    ci
    }
    
confint.polr <- function (object, parm, level=0.95, ...){
    # adapted from stats:::confint.lm
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm))
        parm <- seq(along = pnames)
    else if (is.character(parm))
        parm <- match(parm, pnames, nomatch = 0)
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(round(100 * a, 1), "%")
    ci <- array(NA, dim = c(length(parm), 2), dimnames = list(pnames[parm],
        pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    fac <- qnorm(a)
    ci[] <- cf[parm] + ses %o% fac
    ci
    }

confint.multinom <- function (object, parm, level=0.95, ...){
    # adapted from stats:::confint.lm
    cf <- coef(object)
    if (is.vector(cf)) cf <- matrix(cf, nrow=1,
        dimnames=list(MLM.1$lev[2], names(cf)))
    pnames <- colnames(cf)
    if (missing(parm))
        parm <- seq(along = pnames)
    else if (is.character(parm))
        parm <- match(parm, pnames, nomatch = 0)
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    ses <- matrix(sqrt(diag(vcov(object))),
        ncol=ncol(cf), byrow=TRUE)[,parm, drop=FALSE]
    cf <- cf[,parm, drop=FALSE]
    fac <- qnorm(a)
    ci <- abind(cf + fac[1]*ses, cf + fac[2]*ses, along=3)
    dimnames(ci)[[3]] <- paste(round(100 * a, 1), "%")
    aperm(ci, c(2,3,1))
    }

    # wrapper function for histograms

Hist <- function(x, scale=c("frequency", "percent", "density"), ...){
    xlab <- deparse(substitute(x))
    x <- na.omit(x)
    scale <- match.arg(scale)
    if (scale == "frequency") hist(x, xlab=xlab, main="",  ...)
    else if (scale == "density") hist(x, freq=FALSE, xlab=xlab, main="", ...)
    else {
        n <- length(x)
        hist(x, axes=FALSE, xlab=xlab, ylab="Percent", main="", ...)
        axis(1)
        max <- ceiling(10*par("usr")[4]/n)
        at <- if (max <= 3) (0:(2*max))/20
                else (0:max)/10
        axis(2, at=at*n, labels=at*100)
        }  
    box()   
    abline(h=0, col="gray") 
    invisible(NULL)
    }

stem.leaf <- function(data, unit, m, Min, Max, rule.line=c("Dixon", "Velleman", "Sturges"),
     style=c("Tukey", "bare"), trim.outliers=TRUE, depths=TRUE, reverse.negative.leaves=TRUE){
#Author:  Peter Wolf 05/2003  (modified slightly by J. Fox, 20 July 03)
    rule.line <- match.arg(rule.line)
    style <- match.arg(style)
    n <- length(data<-sort(data))
    row.max <- floor(  c(Dixon   =10*log(n,10),
                        Velleman=2*sqrt(n),
                        Sturges =1+log(n,2)        ))[rule.line]
    stats <- boxplot(data, plot=FALSE)
    if(missing(Min)) Min <- if (trim.outliers) stats$stats[1,1] else min(data, na.rm=TRUE)
    if(missing(Max)) Max <- if (trim.outliers) stats$stats[5,1] else max(data, na.rm=TRUE)
    spannweite.red <- Max - Min
    zeilen.intervall.laenge<-spannweite.red / row.max
    factor <- if(missing(unit)) 10^ceiling(log(zeilen.intervall.laenge,10))
                else 10^round(log(unit*10,10))
    z <- zeilen.intervall.laenge/factor  # z in (0.1 ,1]
    delta.tick <- c(.2,.2,.5,1)[sum(z > c(0,.1,.2,.5))]
    if(missing(m)) m <- round(1/delta.tick) else delta.tick <- 1/m
    data.tr <- data/factor
    Min.tr <- Min/factor
    Max.tr <- Max/factor
    spannweite.red <- Max.tr - Min.tr
    sk.min <-  floor(Min.tr)
    sk.max <- ceiling(Max.tr)
    skala <- seq(sk.min, sk.max, by=delta.tick)
    if(sk.min < 0) skala <- c(sk.min-delta.tick, skala)
    if(sk.max < 0) skala <- skala[-length(skala)]
    lo.limit <- if (trim.outliers) skala[1] else -Inf
    lo.log   <- if(skala[1] <  0) data.tr <= lo.limit else data.tr <  lo.limit
    n.sk <- length(skala)
    hi.limit <- if (trim.outliers) skala[n.sk] + delta.tick else Inf
    hi.log   <- if(skala[n.sk] >= 0) data.tr >= hi.limit else data.tr >  hi.limit
    n.lower.extr.values <- sum(lo.log); n.upper.extr.values <- sum(hi.log)
    if(0 < n.lower.extr.values){
        lower.line<- paste("LO:", paste(data[lo.log],collapse=" "))
        }
    if(0 < n.upper.extr.values){
        upper.line<- paste("HI:", paste(data[hi.log],collapse=" "))
        }
    data.tr.red <-data.tr[(!lo.log)&(!hi.log)]
    stem <- ifelse(data.tr.red < 0, ceiling(data.tr.red), floor(data.tr.red) )
    leaf <- floor(abs(data.tr.red*10 - stem*10))
    class.of.data.tr <- unlist(c(
        sapply(data.tr.red[data.tr.red < 0],
            function(x, sk) length(sk) - sum(-sk <= -x), skala)
            ,sapply(data.tr.red[data.tr.red>=0],
            function(x,sk) sum(sk <= x), skala)
        ))
    class.of.data.tr  <- c(1:length(skala), class.of.data.tr)
    class.negative <- skala < 0
    leaf.grouped      <- split(c(rep(-1, length(skala)), leaf), class.of.data.tr)
    leaf.grouped      <- lapply(leaf.grouped, function(x){ sort(x[-1]) })
    if (reverse.negative.leaves){
        for (i in seq(class.negative))
            if (class.negative[i]) leaf.grouped[[i]] <- rev(leaf.grouped[[i]])
        }
    leaf.grouped.ch <- paste("|",unlist(lapply(leaf.grouped,paste,collapse="")))
    class.neg.zero <- floor(skala) == -1
    line.names <- skala
    line.names[class.negative] <- line.names[class.negative] + 1
    line.names <- as.character(floor(line.names))
    line.names[class.neg.zero] <- "-0"
    if(style=="Tukey"){
        switch(as.character(m),
        "1"={},
        "2"={
                h<-round(2*(skala%%1)) #; line.names[h!=0] <- ""
                line.names<-paste(line.names,
                        ifelse(skala<0,c(".","*")[1+h],c("*",".")[1+h]),sep="")
            },
        "5"={
                h<-round(5*(skala%%1)); line.names[h>0 & h<4] <- ""
                line.names<-paste(line.names, ifelse(skala<0,
                                c(".","s","f","t","*")[1+h],
                                c("*","t","f","s",".")[1+h]), sep="")
            }
            )
        }
    ragged.left<-function(ch.lines){
        max.n <-max(n.lines<-nchar(ch.lines))
        h     <-paste(rep(" ",max.n),collapse="")
        ch.lines <-paste( substring(h,1,1+max.n-n.lines), ch.lines)
        ch.lines
        }
    line.names <- ragged.left(line.names)
    n.class <- unlist(lapply(leaf.grouped, length))
    select <- (cumsum(n.class) > 0) & rev((cumsum(rev(n.class)) > 0))
    depth    <-    cumsum(n.class)          + n.lower.extr.values
    depth.rev <- rev(cumsum(rev(n.class))     + n.upper.extr.values)
    uplow <- depth >= depth.rev
    pos.median <- which(uplow)[1] + (-1:0)
    h <- abs(depth[pos.median]-depth.rev[pos.median])
    pos.median <- pos.median[1]+(h[1]>h[2])
    depth[uplow] <- depth.rev[uplow]
    depth <- paste(depth,"")
    depth[pos.median] <- paste("(",n.class[pos.median],")",sep="")
    depth[n.class == 0] <- " "
    depth <- if (depths) ragged.left(depth) else ""
    info<-     c(  paste("1 | 2: represents",1.2*factor),
                paste(" leaf unit:",factor/10),
                paste("         n:",n     ),
                "")
    stem <- paste(depth, line.names, leaf.grouped.ch)
    if ((style != "Tukey") || (m != 5) || (sum(select) > 4)) stem <- stem[select]
    if(exists("lower.line")) stem <- c(lower=lower.line, stem)
    if(exists("upper.line")) stem <- c(stem, upper=upper.line)
    result <- list(info=info, stem=stem)
    class(result) <- "stem.leaf"
    result
    }
    
print.stem.leaf <- function(x, ...){
    for(i in seq(x)) cat(x[[i]],sep="\n")
    invisible(x)
    }

plotMeans <- function(response, factor1, factor2, error.bars = c("se", "sd", "conf.int", "none"),
    level=0.95, xlab=deparse(substitute(factor1)), ylab=paste("mean of", deparse(substitute(response))), 
    legend.lab=deparse(substitute(factor2)), main="Plot of Means",
    pch=1:n.levs.2, lty=1:n.levs.2, col=palette()){
    if (!is.numeric(response)) stop(gettextRcmdr("Argument response must be numeric."))
    xlab # force evaluation
    ylab
    legend.lab
    error.bars <- match.arg(error.bars)
    if (missing(factor2)){
        if (!is.factor(factor1)) stop(gettextRcmdr("Argument factor1 must be a factor."))
        valid <- complete.cases(factor1, response)
        factor1 <- factor1[valid]
        response <- response[valid]
        means <- tapply(response, factor1, mean)
        sds <- tapply(response, factor1, sd)
        ns <- tapply(response, factor1, length)
        if (error.bars == "se") sds <- sds/sqrt(ns)
        if (error.bars == "conf.int") sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
        yrange <-  if (error.bars != "none") c( min(means - sds), max(means + sds)) else range(means)
        levs <- levels(factor1)
        n.levs <- length(levs)
        plot(c(1, n.levs), yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE, main=main)
        points(1:n.levs, means, type="b", pch=16, cex=2)
        box()
        axis(2)
        axis(1, at=1:n.levs, labels=levs)
        if (error.bars != "none") arrows(1:n.levs, means - sds, 1:n.levs, means + sds, 
            angle=90, lty=2, code=3, length=0.125)
        }
    else {
        if (!(is.factor(factor1) | is.factor(factor2))) stop(gettextRcmdr("Arguments factor1 and factor2 must be factors."))
        valid <- complete.cases(factor1, factor2, response)
        factor1 <- factor1[valid]
        factor2 <- factor2[valid]
        response <- response[valid]
        means <- tapply(response, list(factor1, factor2), mean)
        sds <- tapply(response, list(factor1, factor2), sd)
        ns <- tapply(response, list(factor1, factor2), length)
        if (error.bars == "se") sds <- sds/sqrt(ns)
        if (error.bars == "conf.int") sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
        yrange <-  if (error.bars != "none") c( min(means - sds), max(means + sds)) else range(means)
        levs.1 <- levels(factor1)
        levs.2 <- levels(factor2)
        n.levs.1 <- length(levs.1)
        n.levs.2 <- length(levs.2)
        if (n.levs.2 > length(col)) stop(sprintf(gettextRcmdr("Number of groups for factor2, %d, exceeds number of distinct colours, %d."), n.levs.2, length(col)))
        plot(c(1, n.levs.1 + 1), yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE, main=main)
        box()
        axis(2)
        axis(1, at=1:n.levs.1, labels=levs.1)
        for (i in 1:n.levs.2){
            points(1:n.levs.1, means[, i], type="b", pch=pch[i], cex=2, col=col[i], lty=lty[i])
            if (error.bars != "none") arrows(1:n.levs.1, means[, i] - sds[, i], 
                1:n.levs.1, means[, i] + sds[, i], angle=90, code=3, col=col[i], lty=lty[i], length=0.125)
            }
        x.posn <- n.levs.1 + 0.25
        y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3,4)])
        text(x.posn, y.posn, legend.lab, adj=c(0, -.5))
        legend(x.posn, y.posn, levs.2, pch=pch, col=col, lty=lty)
        }
    invisible(NULL)
    }

bin.var <- function (x, bins=4, method=c("intervals", "proportions", "natural"), labels=FALSE){
    method <- match.arg(method)
# Author: Dan Putler (revision by J. Fox, 5 Dec 04)
    if(length(x) < bins) {
      stop(gettextRcmdr("The number of bins exceeds the number of data values"))
        }
    x <- if(method == "intervals") cut(x, bins, labels=labels)
        else if (method == "proportions") cut(x, quantile(x, probs=seq(0,1,1/bins), na.rm=TRUE),
            include.lowest = TRUE, labels=labels)
        else {
            xx <- na.omit(x)
            breaks <- c(min(xx), tapply(xx, KMeans(xx, bins)$cluster, max))
            cut(x, breaks, include.lowest=TRUE, labels=labels)
            }
    as.factor(x)
    }
    
# 3D scatterplots and point identification via rgl

scatter3d <- function(x, y, z, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
                      zlab=deparse(substitute(z)), revolutions=0, bg.col=c("white", "black"), 
                      axis.col=if (bg.col == "white") "black" else "white",
                      surface.col=c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
                      neg.res.col="red", pos.res.col="green", point.col="yellow",
                      text.col=axis.col, grid.col=if (bg.col == "white") "black" else "gray",
                      fogtype=c("exp2", "linear", "exp", "none"),
                      residuals=(length(fit) == 1), surface=TRUE, grid=TRUE, grid.lines=26,
                      df.smooth=NULL, df.additive=NULL,
                      sphere.size=1, threshold=0.01, speed=1, fov=60,
                      fit="linear", groups=NULL, parallel=TRUE, model.summary=FALSE){
    require(rgl)
    require(mgcv)
    summaries <- list()
    if ((!is.null(groups)) && (nlevels(groups) > length(surface.col))) 
        stop(sprintf(gettextRcmdr("Number of groups (%d) exceeds number of colors (%d)."),
            nlevels(groups), length(surface.col)))
    if ((!is.null(groups)) && (!is.factor(groups))) stop(gettextRcmdr("groups variable must be a factor."))
    bg.col <- match.arg(bg.col)
    fogtype <- match.arg(fogtype)
    if ((length(fit) > 1) && residuals && surface)
        stop(gettextRcmdr("cannot plot both multiple surfaces and residuals"))
    xlab  # cause these arguments to be evaluated
    ylab
    zlab
    rgl.clear()
    rgl.viewpoint(fov=fov)
    rgl.bg(col=bg.col, fogtype=fogtype)
    valid <- if (is.null(groups)) complete.cases(x, y, z)
        else complete.cases(x, y, z, groups)
    x <- x[valid]
    y <- y[valid]
    z <- z[valid]
    if (!is.null(groups)) groups <- groups[valid]
    x <- (x - min(x))/(max(x) - min(x))
    y <- (y - min(y))/(max(y) - min(y))
    z <- (z - min(z))/(max(z) - min(z))
    size <- sphere.size*((100/length(x))^(1/3))*0.015
    if (is.null(groups)){
        if (size > threshold) rgl.spheres(x, y, z, color=point.col, radius=size)
            else rgl.points(x, y, z, color=point.col)
            }
    else {
        if (size > threshold) rgl.spheres(x, y, z, color=surface.col[as.numeric(groups)], radius=size)
            else rgl.points(x, y, z, color=surface.col[as.numeric(groups)])
            }
    rgl.lines(c(0,1), c(0,0), c(0,0), color=axis.col)
    rgl.lines(c(0,0), c(0,1), c(0,0), color=axis.col)
    rgl.lines(c(0,0), c(0,0), c(0,1), color=axis.col)
    rgl.texts(1, 0, 0, xlab, adj=1, color=text.col)
    rgl.texts(0, 1, 0, ylab, adj=1, color=text.col)
    rgl.texts(0, 0, 1, zlab, adj=1, color=text.col)
    if (surface){
        vals <- seq(0, 1, length=grid.lines)
        dat <- expand.grid(x=vals, z=vals)
        for (i in 1:length(fit)){
            f <- match.arg(fit[i], c("linear", "quadratic", "smooth", "additive"))
#            vals <- seq(0, 1, length=grid.lines)
#            dat <- expand.grid(x=vals, z=vals)
            if (is.null(groups)){
                mod <- switch(f,
                    linear = lm(y ~ x + z),
                    quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2)),
                    smooth = if (is.null(df.smooth)) gam(y ~ s(x, z))
                        else gam(y ~ s(x, z, fx=TRUE, k=df.smooth)),
                    additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z))
                        else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                            s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)))
                    )
                if (model.summary) summaries[[f]] <- summary(mod)
                yhat <- matrix(predict(mod, newdata=dat), grid.lines, grid.lines)
                rgl.surface(vals, vals, yhat, color=surface.col[i], alpha=0.5, lit=FALSE)
                if(grid) rgl.surface(vals, vals, yhat, color=grid.col, alpha=0.5, lit=FALSE, front="lines", back="lines")
                if (residuals){
                    n <- length(y)
                    fitted <- fitted(mod)
                    colors <- ifelse(residuals(mod) > 0, pos.res.col, neg.res.col)
                    rgl.lines(as.vector(rbind(x,x)), as.vector(rbind(y,fitted)), as.vector(rbind(z,z)),
                        color=as.vector(rbind(colors,colors)))
                    }
                }
            else{
                if (parallel){
                    mod <- switch(f,
                        linear = lm(y ~ x + z + groups),
                        quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2) + groups),
                        smooth = if (is.null(df.smooth)) gam(y ~ s(x, z) + groups)
                            else gam(y ~ s(x, z, fx=TRUE, k=df.smooth) + groups),
                        additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z) + groups)
                            else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                                s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)) + groups)
                        )
                    if (model.summary) summaries[[f]] <- summary(mod)
                    levs <- levels(groups)
                    for (j in 1:length(levs)){
                        group <- levs[j]
                        select.obs <- groups == group
                        yhat <- matrix(predict(mod, newdata=cbind(dat, groups=group)), grid.lines, grid.lines)
                        rgl.surface(vals, vals, yhat, color=surface.col[j], alpha=0.5, lit=FALSE)
                        if (grid) rgl.surface(vals, vals, yhat, color=grid.col, alpha=0.5, lit=FALSE, front="lines", back="lines")
                        rgl.texts(0, predict(mod, newdata=data.frame(x=0, z=0, groups=group)), 0,
                            paste(group, " "), adj=1, color=surface.col[j])
                        if (residuals){
                            yy <- y[select.obs]
                            xx <- x[select.obs]
                            zz <- z[select.obs]
                            fitted <- fitted(mod)[select.obs]
                            rgl.lines(as.vector(rbind(xx,xx)), as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)),
                                col=surface.col[j])
                            }
                        }
                    }
                else {
                    levs <- levels(groups)
                    for (j in 1:length(levs)){
                        group <- levs[j]
                        select.obs <- groups == group
                        mod <- switch(f,
                            linear = lm(y ~ x + z, subset=select.obs),
                            quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2), subset=select.obs),
                            smooth = if (is.null(df.smooth)) gam(y ~ s(x, z), subset=select.obs)
                                else gam(y ~ s(x, z, fx=TRUE, k=df.smooth), subset=select.obs),
                            additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z), subset=select.obs)
                                else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                                    s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)), subset=select.obs)
                            )
                        if (model.summary) summaries[[paste(f, ".", group, sep="")]] <- summary(mod)
                        yhat <- matrix(predict(mod, newdata=dat), grid.lines, grid.lines)
                        rgl.surface(vals, vals, yhat, color=surface.col[j], alpha=0.5, lit=FALSE)
                        rgl.surface(vals, vals, yhat, color=grid.col, alpha=0.5, lit=FALSE, front="lines", back="lines")
                        rgl.texts(0, predict(mod, newdata=data.frame(x=0, z=0, groups=group)), 0,
                            paste(group, " "), adj=1, color=surface.col[j])
                        if (residuals){
                            yy <- y[select.obs]
                            xx <- x[select.obs]
                            zz <- z[select.obs]
                            fitted <- fitted(mod)
                            rgl.lines(as.vector(rbind(xx,xx)), as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)),
                                col=surface.col[j])
                            }
                        }
                    }
                }
            }
        }
    if (revolutions > 0) {
        for (i in 1:revolutions){
            for (angle in seq(1, 360, length=360/speed)) rgl.viewpoint(-angle, fov=fov)
            }
        }
    if (model.summary) return(summaries) else return(invisible(NULL))
    }

# the following function is a slight modification of rgl.select3d() in the rgl package,
#   altered to pass through arguments (via ...) to rgl.select()

Rcmdr.select3d <-
function (...) 
{
    rgl:::.check3d()
    rect <- rgl:::rgl.select(...)
    llx <- rect[1]
    lly <- rect[2]
    urx <- rect[3]
    ury <- rect[4]
    if (llx > urx) {
        temp <- llx
        llx <- urx
        urx <- temp
    }
    if (lly > ury) {
        temp <- lly
        lly <- ury
        ury <- temp
    }
    proj <- rgl:::rgl.projection()
    function(x, y, z) {
        pixel <- rgl.user2window(x, y, z, proj = proj)
        apply(pixel, 1, function(p) (llx <= p[1]) && (p[1] <= 
            urx) && (lly <= p[2]) && (p[2] <= ury) && (0 <= p[3]) && 
            (p[3] <= 1))
    }
}

identify3d  <-
function (x, y, z, groups = NULL, labels = 1:length(x), col = c("blue", 
    "green", "orange", "magenta", "cyan", "red", "yellow", "gray"), 
    offset = ((100/length(x))^(1/3)) * 0.02) 
{
    valid <- if (is.null(groups)) 
        complete.cases(x, y, z)
    else complete.cases(x, y, z, groups)
    x <- x[valid]
    y <- y[valid]
    z <- z[valid]
    labels <- labels[valid]
    x <- (x - min(x))/(max(x) - min(x))
    y <- (y - min(y))/(max(y) - min(y))
    z <- (z - min(z))/(max(z) - min(z))
    rgl.bringtotop()
    identified <- character(0)
    groups <- if (!is.null(groups)) 
        as.numeric(groups[valid])
    else rep(1, length(x))
    repeat {
        f <- Rcmdr.select3d(button="middle")
        which <- f(x, y, z)
        if (!any(which)) 
            break
        rgl.texts(x[which], y[which] + offset, z[which], labels[which], 
            color = col[groups][which])
        identified <- c(identified, labels[which])
    }
    unique(identified)
}

    # Pager

# this is slightly modified from tkpager to use the Rcmdr monospaced font
#   and a white background
    
RcmdrPager <- function (file, header, title, delete.file) 
{
    for (i in seq(along = file)) {
        zfile <- file[[i]]
        tt <- tktoplevel()
        tkwm.title(tt, if (length(title)) 
            title[(i - 1)%%length(title) + 1]
        else "")
        txt <- tktext(tt, bg = "white", font = getRcmdr("logFont"))
        scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(txt, 
            ...))
        tkconfigure(txt, yscrollcommand = function(...) tkset(scr, 
            ...))
        tkpack(txt, side = "left", fill = "both", expand = TRUE)
        tkpack(scr, side = "right", fill = "y")
        chn <- tkcmd("open", zfile)
        tkinsert(txt, "end", header[[i]])
        tkinsert(txt, "end", gsub("_\b", "", tclvalue(tkcmd("read", 
            chn))))
        tkcmd("close", chn)
        tkconfigure(txt, state = "disabled")
        tkmark.set(txt, "insert", "0.0")
        tkfocus(txt)
        if (delete.file) 
            tkcmd("file", "delete", zfile)
    }
}

    # help functions
    

    
helpCommander <- function() {
    if (as.numeric(R.Version()$major) >= 2) print(help("Commander"))
    else help("Commander")
    }

helpAboutCommander <- function() {
    if (as.numeric(R.Version()$major) >= 2) print(help("Rcmdr"))
    else help("Rcmdr")
    }

browseManual <- function() {
    browseURL(paste(file.path(.path.package(package="Rcmdr")[1], "doc"), 
        "/Getting-Started-with-the-Rcmdr.pdf", sep=""))
    }


    
    # functions for building dialog boxes
    
# the following function is slightly modified from Thomas Lumley, 
#   "Programmer's Niche: Macros in R," R-News, Sept. 2001, Vol. 1, No. 3, pp.11-13.
defmacro <- function(..., expr){
    expr <- substitute(expr)
    len <- length(expr)
    expr[3:(len+1)] <- expr[2:len]
    ## delete "macro" variables starting in ..
    expr[[2]] <- quote(on.exit(remove(list=objects(pattern="^\\.\\.", all.names=TRUE))))
    a <- substitute(list(...))[-1]
    ## process the argument list
    nn <- names(a)
    if (is.null(nn)) nn <- rep("", length(a))
    for (i in seq(length=length(a))){
        if (nn[i] == "") {
            nn[i] <- paste(a[[i]])
            msg <- paste(a[[i]], gettext("not supplied", domain="R-Rcmdr"))
            a[[i]] <- substitute(stop(foo), list(foo = msg))
            }
        }
    names(a) <- nn
    a <- as.list(a)
    ff <- eval(substitute(
        function(){
            tmp <- substitute(body)
            eval(tmp, parent.frame())
            },
        list(body = expr)))
    ## add the argument list
    formals(ff) <- a
    ## create a fake source attribute
    mm <- match.call()
    mm$expr <- NULL
    mm[[1]] <- as.name("macro")
    expr[[2]] <- NULL # get "local" variable removal out of source
    attr(ff, "source") <- c(deparse(mm), deparse(expr))
    ## return the macro
    ff
    }

OKCancelHelp <- defmacro(window=top, helpSubject=NULL, model=FALSE,
    expr={
        buttonsFrame <- tkframe(window, borderwidth=5)
        OKbutton <- tkbutton(buttonsFrame, text=gettextRcmdr("OK"), fg="darkgreen", width="12", command=onOK, default="active",
            borderwidth=3)
        onCancel <- function() {
            if (model) putRcmdr("modelNumber", getRcmdr("modelNumber") - 1)
            if (GrabFocus()) tkgrab.release(window)
            tkdestroy(window)  
            tkfocus(CommanderWindow())
            }
        cancelButton <- tkbutton(buttonsFrame, text=gettextRcmdr("Cancel"), fg="red", width="12", command=onCancel, borderwidth=3)
        if (!is.null(helpSubject)){
            onHelp <- function() {
                if (GrabFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
                if (as.numeric(R.Version()$major) >= 2) print(help(helpSubject))
                else help(helpSubject)
                }
            helpButton <- tkbutton(buttonsFrame, text=gettextRcmdr("Help"), width="12", command=onHelp, borderwidth=3)
            }       
        tkgrid(OKbutton, tklabel(buttonsFrame, text="  "), cancelButton, tklabel(buttonsFrame, text="            "), 
            if (!is.null(helpSubject)) helpButton, sticky="w")
        })

subOKCancelHelp <- defmacro(window=subdialog, helpSubject=NULL,
    expr={
        subButtonsFrame <- tkframe(window, borderwidth=5)
        subOKbutton <- tkbutton(subButtonsFrame, text=gettextRcmdr("OK"), fg="darkgreen", width="12", command=onOKsub, default="active",
            borderwidth=3)
        onCancelSub <- function() {
            if (GrabFocus()) tkgrab.release(window)
            tkdestroy(window)  
            tkfocus(CommanderWindow())
            }
        subCancelButton <- tkbutton(subButtonsFrame, text=gettextRcmdr("Cancel"), fg="red", width="12", command=onCancelSub, 
            borderwidth=3)
        if (!is.null(helpSubject)){
            onHelpSub <- function(){
                if (GradFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
                if (as.numeric(R.Version()$major) >= 2) print(help(helpSubject))
                else help(helpSubject)
                }
            subHelpButton <- tkbutton(subButtonsFrame, text=gettextRcmdr("Help"), width="12", command=onHelpSub, borderwidth=3)
            }       
        tkgrid(subOKbutton, tklabel(subButtonsFrame, text="  "), subCancelButton, 
            tklabel(subButtonsFrame, text="            "), if (!is.null(helpSubject)) subHelpButton, sticky="w")
        })

checkActiveDataSet <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(CommanderWindow())
        FALSE
        }
    else TRUE
    }
    
checkActiveModel <- function(){
    if (activeModel() == FALSE) {
        tkfocus(CommanderWindow())
        FALSE
        }
    else TRUE
    }
    
checkFactors <- function(n=1){
    if (length(Factors()) < n){
        if (n > 1)
            Message(message=sprintf(gettextRcmdr("There fewer than %d factors in the active data set."), n),
                    type="error")
        else Message(message=gettextRcmdr("There are no factors in the active data set."),
                    type="error")
        tkfocus(CommanderWindow())
        FALSE
        }
    else TRUE
    }
    
checkTwoLevelFactors <- function(n=1){
    if (length(TwoLevelFactors()) < n){
        if (n > 1)
            Message(message=sprintf(gettextRcmdr("There fewer than %d two-level factors in the active data set."), n),
                    type="error")
        else Message(message=gettextRcmdr("There are no two-level factors in the active data set."),
                    type="error")
        tkfocus(CommanderWindow())
        FALSE
        }
    else TRUE
    }
    
checkNumeric <- function(n=1){
    if (length(Numeric()) < n){
        if (n > 1)
            Message(message=sprintf(gettextRcmdr("There fewer than %d numeric variables in the active data set."), n),
                    type="error")
        else Message(message=gettextRcmdr("There are no numeric variables in the active data set."),
                    type="error")
        tkfocus(CommanderWindow())
        FALSE
        }
    else TRUE
    }
    
checkVariables <- function(n=1){
    if (length(Variables()) < n){
        if (n > 1)
            Message(message=sprintf(gettextRcmdr("There fewer than %d variables in the active data set."), n), 
                    type="error")
        else Message(message=gettextRcmdr("There are no variables in the active data set."),
                    type="error")
        tkfocus(CommanderWindow())
        FALSE
        }
    else TRUE
    }

commanderPosition <- function (){
   ID <- CommanderWindow()$ID
   as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
       tclvalue(.Tcl(paste("winfo rooty", ID)))))
   }

initializeDialog <- defmacro(window=top, title="", offset=10, preventCrisp=FALSE,
    expr={
        if ((!preventCrisp) && getRcmdr("crisp.dialogs")) tclServiceMode(on=FALSE)
        window <- tktoplevel(borderwidth=10)
        tkwm.title(window, title)
        position <- if (is.SciViews()) -1 else commanderPosition() # +PhG
        position <- if (any(position < 0)) "-50+50"
            else paste("+", paste(offset + position, collapse="+"), sep="")
        tkwm.geometry(window, position)
        }
    )

closeDialog <- defmacro(window=top, release=TRUE,
    expr={
        if (release && GrabFocus()) tkgrab.release(window)
        tkdestroy(window)
        }
    )

dialogSuffix <- defmacro(window=top, onOK=onOK, rows=1, columns=1, focus=top,
    bindReturn=TRUE, preventGrabFocus=FALSE, preventDoubleClick=FALSE,
    preventCrisp=FALSE,
    expr={
        for (row in 0:(rows-1)) tkgrid.rowconfigure(window, row, weight=0)
        for (col in 0:(columns-1)) tkgrid.columnconfigure(window, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(window, 0, 0)
        if (bindReturn) tkbind(window, "<Return>", onOK)
        if (getRcmdr("double.click") && (!preventDoubleClick)) tkbind(window, "<Double-ButtonPress-1>", onOK)
        tkwm.deiconify(window)
        # focus grabs appear to cause problems for some dialogs
        if (GrabFocus() && (!preventGrabFocus)) tkgrab.set(window)
        tkfocus(focus)
        tkwait.window(window)
        if ((!preventCrisp) && getRcmdr("crisp.dialogs")) tclServiceMode(on=TRUE)
        }
    )
            
variableListBox <- function(parentWindow, variableList=Variables(), bg="white",
    selectmode="single", export="FALSE", initialSelection=NULL, listHeight=4, title){
    if (selectmode == "multiple") selectmode <- getRcmdr("multiple.select.mode")
    frame <- tkframe(parentWindow)
    listbox <- tklistbox(frame, height=min(listHeight, length(variableList)),
        selectmode=selectmode, background=bg, exportselection=export)
    scrollbar <- tkscrollbar(frame, repeatinterval=5, command=function(...) tkyview(listbox, ...))
    tkconfigure(listbox, yscrollcommand=function(...) tkset(scrollbar, ...))
    for (var in variableList) tkinsert(listbox, "end", var)
    if (!is.null(initialSelection)) tkselection.set(listbox, initialSelection)  
    tkgrid(tklabel(frame, text=title, fg="blue"), columnspan=2, sticky="w")
    tkgrid(listbox, scrollbar, sticky="nw")
    tkgrid.configure(scrollbar, sticky="wns")
    tkgrid.configure(listbox, sticky="ew")
    result <- list(frame=frame, listbox=listbox, scrollbar=scrollbar, 
        selectmode=selectmode, varlist=variableList)
    class(result) <- "listbox"
    result
    }      
            
getSelection <- function(object) UseMethod("getSelection")

getSelection.listbox <- function(object){
    object$varlist[as.numeric(tkcurselection(object$listbox)) + 1]
    }
    
getFrame <- function(object) UseMethod("getFrame")

getFrame.listbox <- function(object){
    object$frame
    }

radioButtons <- defmacro(window=top, name, buttons, values=NULL, initialValue=..values[1], labels, title,
    expr={
        ..values <- if (is.null(values)) buttons else values
        ..frame <- paste(name, "Frame", sep="")
        assign(..frame, tkframe(window))
        ..variable <- paste(name, "Variable", sep="")
        assign(..variable, tclVar(initialValue))
        tkgrid(tklabel(eval(parse(text=..frame)), text=title, fg="blue"), columnspan=2, sticky="w")
        for (i in 1:length(buttons)) {
            ..button <- paste(buttons[i], "Button", sep="")
            assign(..button, 
                tkradiobutton(eval(parse(text=..frame)), variable=eval(parse(text=..variable)), value=..values[i]))
            tkgrid(tklabel(eval(parse(text=..frame)), text=labels[i], justify="left"), eval(parse(text=..button)), sticky="w")
            }
        }
    )
            
                    
checkBoxes <- defmacro(window=top, frame, boxes, initialValues=NULL, labels,
    expr={
        ..initialValues <- if (is.null(initialValues)) rep("1", length(boxes)) else initialValues
        assign(frame, tkframe(window))
        ..variables <- paste(boxes, "Variable", sep="")
        for (i in 1:length(boxes)) {
            assign(..variables[i], tclVar(..initialValues[i]))
            ..checkBox <- paste(boxes[i], "CheckBox", sep="")
            assign(..checkBox, 
                tkcheckbutton(eval(parse(text=frame)), variable=eval(parse(text=..variables[i]))))
            tkgrid(tklabel(eval(parse(text=frame)), text=labels[i]), eval(parse(text=..checkBox)), sticky="w")
            }
        }
    )

checkReplace <- function(name, type=gettextRcmdr("Variable")){
    RcmdrTkmessageBox(message=sprintf(gettextRcmdr("%s %s already exists.\nOverwrite %s?"),
        type, name, tolower(type)), icon="warning", type="yesno", default="no")
    }

errorCondition <- defmacro(window=top, recall=NULL, message, model=FALSE,
    expr={
        if (model) putRcmdr("modelNumber", getRcmdr("modelNumber") - 1)
        if (GrabFocus()) tkgrab.release(window)
        tkdestroy(window)
        Message(message=message, type="error")
        if (!is.null(recall)) recall()
        else tkfocus(CommanderWindow())
        })

subsetBox <- defmacro(window=top, model=FALSE, 
    expr={
            subsetVariable <- if (model){
                if (currentModel && currentFields$subset != "") 
                    tclVar(currentFields$subset) else tclVar(gettextRcmdr("<all valid cases>"))
                }
            else tclVar(gettextRcmdr("<all valid cases>"))
            subsetFrame <- tkframe(window)
            subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
            subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
                repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
            tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
            tkgrid(tklabel(subsetFrame, text=gettextRcmdr("Subset expression"), fg="blue"), sticky="w")
            tkgrid(subsetEntry, sticky="w")
            tkgrid(subsetScroll, sticky="ew")
            })

groupsBox <- defmacro(recall=NULL, label=gettextRcmdr("Plot by:"), initialLabel=gettextRcmdr("Plot by groups"),
    plotLinesByGroup=FALSE, positionLegend=FALSE, plotLinesByGroupsText=gettextRcmdr("Plot lines by group"),
    expr={
        env <- environment()
        .groups <- FALSE
        .linesByGroup <- FALSE
        .groupsLabel <- tclVar(paste(initialLabel, "...", sep=""))
        .factors <- Factors()
        onGroups <- function(){
            if (length(.factors) == 0){
                errorCondition(recall=recall, message=gettextRcmdr("There no factors in the active data set.")) 
                return()
                }
            initializeDialog(subdialog, title=gettextRcmdr("Groups"))
            groupsBox <- variableListBox(subdialog, .factors, title=gettextRcmdr("Groups variable (pick one)"))
            if (plotLinesByGroup){
                linesByGroupFrame <- tkframe(subdialog)
                linesByGroup <- tclVar("1")
                linesCheckBox <- tkcheckbutton(linesByGroupFrame, variable=linesByGroup)
                tkgrid(tklabel(linesByGroupFrame, text=plotLinesByGroupsText), linesCheckBox, sticky="w")
                }
            onOKsub <- function() {
                groups <- getSelection(groupsBox)
                if (length(groups) == 0){
                    assign(".groups", FALSE, envir=env)
                    tclvalue(.groupsLabel) <- paste(initialLabel, "...", sep="")
                    tkconfigure(groupsButton, fg="black")
                    if (GrabFocus()) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    tkwm.deiconify(top)
                    if (GrabFocus()) tkgrab.set(top)
                    tkfocus(top)
                    tkwait.window(top)                
                    return()
                    }
                assign(".groups", groups, envir=env)
                tclvalue(.groupsLabel) <- paste(label, groups)
                tkconfigure(groupsButton, fg="blue")
                if (plotLinesByGroup) {
                    lines <- as.character("1" == tclvalue(linesByGroup))
                    assign(".linesByGroup", lines, envir=env)
                    }
                if (GrabFocus()) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                tkwm.deiconify(top)
                if (GrabFocus()) tkgrab.set(top)
                tkfocus(top)
                tkwait.window(top)
                }
            subOKCancelHelp()
            tkgrid(getFrame(groupsBox), sticky="nw")
            if (plotLinesByGroup) tkgrid(linesByGroupFrame, sticky="w")
            tkgrid(subButtonsFrame, sticky="w")
            if (positionLegend) tkgrid(tklabel(subdialog, text=gettextRcmdr("Position legend with mouse click"), fg="blue"))
            dialogSuffix(subdialog, onOK=onOKsub, rows=3+plotLinesByGroup+positionLegend, columns=2, focus=subdialog)
            }
        groupsFrame <- tkframe(top)
        groupsButton <- tkbutton(groupsFrame, textvariable=.groupsLabel, command=onGroups, borderwidth=3)
        tkgrid(tklabel(groupsFrame, text="    "), groupsButton, sticky="w")
        })

groupsLabel <- defmacro(frame=top, groupsBox=groupsBox, columnspan=1,
    expr={
        groupsFrame <- tkframe(frame)
        groupsLabel <- tklabel(groupsFrame, text=gettextRcmdr("<No groups selected>"))    
        tkgrid(tklabel(groupsFrame, text=gettextRcmdr("Difference: "), fg="blue"), groupsLabel, sticky="w")
        tkgrid(groupsFrame, sticky="w", columnspan=columnspan)
        onSelect <- function(){
            group <- getSelection(groupsBox)
            levels <- eval(parse(text=paste("levels(", ActiveDataSet(), "$", group, ")", sep="")))
            tkconfigure(groupsLabel, text=paste(levels[1], "-", levels[2]))
            }
        tkbind(groupsBox$listbox, "<ButtonRelease-1>", onSelect)
        })

modelFormula <- defmacro(frame=top, hasLhs=TRUE, expr={
    checkAddOperator <- function(rhs){
        rhs.chars <- rev(strsplit(rhs, "")[[1]])
        if (length(rhs.chars) < 1) return(FALSE)
        check.char <- if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                rhs.chars[1] else rhs.chars[2]
        !is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%"))
        }
    .variables <- Variables()
    variables <- paste(.variables, ifelse(is.element(.variables, Factors()), "[factor]", ""))
    xBox <- variableListBox(frame, variables, title=gettextRcmdr("Variables (double-click to formula)"))
    onDoubleClick <- if (!hasLhs){
        function(){
            var <- getSelection(xBox)
            if (length(grep("\\[factor\\]", var)) == 1) var <- sub("\\[factor\\]", "",  var)
            tkfocus(rhsEntry)
            rhs <- tclvalue(rhsVariable)
            rhs.chars <- rev(strsplit(rhs, "")[[1]])
            check.char <- if (length(rhs.chars) > 0){
                if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                    rhs.chars[1] else rhs.chars[2]
                }
                else ""
            tclvalue(rhsVariable) <- if (rhs == "" || 
                is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%")))
                    paste(rhs, var, sep="")
                else paste(rhs, "+", var)
            tkicursor(rhsEntry, "end")
            tkxview.moveto(rhsEntry, "1")
            }
        }
    else{
        function(){
            var <- getSelection(xBox)
            if (length(grep("\\[factor\\]", var)) == 1) var <- sub("\\[factor\\]", "",  var)
            lhs <- tclvalue(lhsVariable)
            if (lhs == "") tclvalue(lhsVariable) <- var
            else {
                tkfocus(rhsEntry)
                rhs <- tclvalue(rhsVariable)
                rhs.chars <- rev(strsplit(rhs, "")[[1]])
                check.char <- if (length(rhs.chars) > 0){
                    if ((rhs.chars[1] != " ") || (length(rhs.chars) == 1)) 
                        rhs.chars[1] else rhs.chars[2]
                    }
                    else ""
                tclvalue(rhsVariable) <- if (rhs == "" || 
                    is.element(check.char, c("+", "*", ":", "/", "-", "^", "(", "%")))
                        paste(rhs, var, sep="")
                    else paste(rhs, "+", var)
                }
            tkicursor(rhsEntry, "end")
            tkxview.moveto(rhsEntry, "1")
            }
        }
    tkbind(xBox$listbox, "<Double-ButtonPress-1>", onDoubleClick)
    onPlus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "+ ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onTimes <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "*", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onColon <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ":", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onSlash <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "/",  sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onIn <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "%in% ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onMinus <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "- ")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onPower <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, "^", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onLeftParen <- function(){
        tkfocus(rhsEntry)
        rhs <- tclvalue(rhsVariable)
        tclvalue(rhsVariable) <- paste(rhs, "(", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    onRightParen <- function(){
        rhs <- tclvalue(rhsVariable)
        if (!checkAddOperator(rhs)) return()
        tclvalue(rhsVariable) <- paste(rhs, ")", sep="")
        tkicursor(rhsEntry, "end")
        tkxview.moveto(rhsEntry, "1")
        }
    outerOperatorsFrame <- tkframe(frame)
    operatorsFrame <- tkframe(outerOperatorsFrame)
    .operatorFont <- getRcmdr("operatorFont")
    plusButton <- tkbutton(operatorsFrame, text="+", width="3", command=onPlus, 
        font=.operatorFont)
    timesButton <- tkbutton(operatorsFrame, text="*", width="3", command=onTimes, 
        font=.operatorFont)
    colonButton <- tkbutton(operatorsFrame, text=":", width="3", command=onColon, 
        font=.operatorFont)
    slashButton <- tkbutton(operatorsFrame, text="/", width="3", command=onSlash, 
        font=.operatorFont)
    inButton <- tkbutton(operatorsFrame, text="%in%", width="3", command=onIn,
        font=.operatorFont)
    minusButton <- tkbutton(operatorsFrame, text="-", width="3", command=onMinus, 
        font=.operatorFont)
    powerButton <- tkbutton(operatorsFrame, text="^", width="3", command=onPower, 
        font=.operatorFont)
    leftParenButton <- tkbutton(operatorsFrame, text="(", width="3", command=onLeftParen, 
        font=.operatorFont)
    rightParenButton <- tkbutton(operatorsFrame, text=")", width="3", command=onRightParen, 
        font=.operatorFont)
    tkgrid(plusButton, timesButton, colonButton, slashButton, inButton, minusButton,
        powerButton, leftParenButton, rightParenButton, sticky="w")
    formulaFrame <- tkframe(frame)
    if (hasLhs){
        tkgrid(tklabel(outerOperatorsFrame, text=gettextRcmdr("Model Formula:     "), fg="blue"), operatorsFrame)
        lhsVariable <- if (currentModel) tclVar(currentFields$lhs) else tclVar("")
        rhsVariable <- if (currentModel) tclVar(currentFields$rhs) else tclVar("")
        rhsEntry <- tkentry(formulaFrame, width="50", textvariable=rhsVariable)
        rhsXscroll <- tkscrollbar(formulaFrame, repeatinterval=10,
            orient="horizontal", command=function(...) tkxview(rhs, ...))
        tkconfigure(rhsEntry, xscrollcommand=function(...) tkset(rhsXscroll, ...))          
        lhsEntry <- tkentry(formulaFrame, width="10", textvariable=lhsVariable)
        lhsScroll <- tkscrollbar(formulaFrame, repeatinterval=5, 
            orient="horizontal", command=function(...) tkxview(lhsEntry, ...))
        tkconfigure(lhsEntry, xscrollcommand=function(...) tkset(lhsScroll, ...))
        tkgrid(lhsEntry, tklabel(formulaFrame, text=" ~    "), rhsEntry, sticky="w")
        tkgrid(lhsScroll, tklabel(formulaFrame, text=""), rhsXscroll, sticky="w")
        tkgrid.configure(lhsScroll, sticky="ew")
        }
    else{
        rhsVariable <- tclVar("")
        rhsEntry <- tkentry(formulaFrame, width="50", textvariable=rhsVariable)
        rhsXscroll <- tkscrollbar(formulaFrame, repeatinterval=10,
            orient="horizontal", command=function(...) tkxview(rhs, ...))
        tkconfigure(rhsEntry, xscrollcommand=function(...) tkset(rhsXscroll, ...))  
        tkgrid(tklabel(formulaFrame, text="   ~ "), rhsEntry, sticky="w")
        tkgrid(tklabel(formulaFrame, text=""), rhsXscroll, sticky="w")
        }
    tkgrid.configure(rhsXscroll, sticky="ew")
    })

exists.method <- function(generic, object, default=TRUE, strict=FALSE){
    classes <- class(object)
    if (default) classes <- c(classes, "default")
    if (strict) classes <- classes[1]
    any(paste(generic, ".", classes, sep="") %in%
        as.character(methods(generic)))
    }

checkMethod <- defmacro(generic, object, message=NULL, default=FALSE, strict=FALSE, reportError=TRUE,
    expr={
        msg <- if (is.null(message)) sprintf(gettextRcmdr("No appropriate %s method exists\nfor a model of this class."), generic)
            else message
        method <- exists.method(generic, eval(parse(text=object)), default=default, strict=strict)
        if ((!method) && reportError) Message(message=msg, type="error")
        method
        }
    )
    
checkClass <- defmacro(object, class, message=NULL,
    expr={
        msg <- if (is.null(message)) sprintf(gettextRcmdr('The model is not of class "%s".'), class)
            else message
       properClass <- eval(parse(text=paste("class(", object, ")")))[1] == class
       if (!properClass) Message(message=msg, type="error")
       properClass
       }
    )
    

# the following function is from John Chambers

isS4object <- function(object) {
     if(length(attr(object, "class"))!= 1)
         return(FALSE)
    !isVirtualClass(getClass(class(object), TRUE)) }


#isS4object <- function(object) {
#    !(length(object) == 1 && class(object) == "character") &&  length(slotNames(object)) != 0
#    }

# the following three functions are slightly adapted with permission from Philippe Grosjean

RcmdrEnv <- function() {
    pos <-  match("RcmdrEnv", search())
    if (is.na(pos)) { # Must create it
        RcmdrEnv <- list()
        attach(RcmdrEnv, pos = length(search()) - 1)
        rm(RcmdrEnv)
        pos <- match("RcmdrEnv", search())
        }
    return(pos.to.env(pos))
    }

putRcmdr <- function(x, value)
    assign(x, value, envir = RcmdrEnv())

getRcmdr <- function(x, mode="any")
    get(x, envir = RcmdrEnv(), mode = mode, inherits = FALSE)
    
RcmdrTclSet <- function(name, value){
    if (is.SciViews()) return()   # + PhG
    name <- ls(unclass(getRcmdr(name))$env)
    tcl("set", name, value)
    }

    
# functions to store or retrieve Rcmdr state information

Variables <- function(names){
    if (missing(names)) getRcmdr("variables")
    else putRcmdr("variables", names)
    }

Numeric <- function(names){
    if (missing(names)) getRcmdr("numeric")
    else putRcmdr("numeric", names)
    }
    
Factors <- function(names){
    if (missing(names)) getRcmdr("factors")
    else putRcmdr("factors", names)
    }

TwoLevelFactors <- function(names){
    if (missing(names)) getRcmdr("twoLevelFactors")
    else putRcmdr("twoLevelFactors", names)
    }
    
ActiveDataSet <- function(name){
    if (missing(name)) getRcmdr(".activeDataSet")
    else putRcmdr(".activeDataSet", name)
    }

ActiveModel <- function(name){
    if (missing(name)) getRcmdr(".activeModel")
    else putRcmdr(".activeModel", name)
    }
    
GrabFocus <- function(value){
    if (missing(value)) getRcmdr("grab.focus")
    else putRcmdr("grab.focus", value)
    }

UpdateModelNumber <- function(increment=1){
    modelNumber <- getRcmdr("modelNumber")
    putRcmdr("modelNumber", modelNumber + increment)
    }
    
CommanderWindow <- function() getRcmdr("commanderWindow")

LogWindow <- function() getRcmdr("logWindow")

OutputWindow <- function() getRcmdr("outputWindow")

MessagesWindow <- function() getRcmdr("messagesWindow")
    
# some predicates for the menu system

activeDataSetP <- function() !is.null(ActiveDataSet())

dataSetsP <- function() !is.null(listDataSets())

numericP <- function(n=1) activeDataSetP() && length(listNumeric()) >= n

factorsP <- function(n=1) activeDataSetP() && length(listFactors()) >= n

twoLevelFactorsP <- function(n=1) activeDataSetP() && length(listTwoLevelFactors()) >= n

modelsP <- function(n=1) activeDataSetP() && length(listAllModels()) >= n

activeModelP <- function() !is.null(ActiveModel())

lmP <- function() activeModelP() && eval(parse(text=paste("class(", ActiveModel(), ")[1] == 'lm'")))

glmP <- function() activeModelP() && eval(parse(text=paste("class(", ActiveModel(), ")[1] == 'glm'")))

hclustSolutionsP <- function() length(listHclustSolutions()) > 0

packageAvailable <- function(name) 0 != length(.find.package(name, quiet=TRUE))

rglLoaded <- function() 0 != length(grep("^rgl", loadedNamespaces()))

activateMenus <- function(){
    for (item in getRcmdr("Menus")){
        if (item$activation()) .Tcl(paste(item$ID, " entryconfigure ", item$position - 1," -state normal", sep=""))
        else .Tcl(paste(item$ID, " entryconfigure ", item$position - 1," -state disabled", sep=""))
        }
    }

    
# for internationalization

gettextRcmdr <- function(...) gettext(..., domain="R-Rcmdr")

English <- function() {
    env <- Sys.getenv()
    names(env) <- toupper(names(env))
    LANG <- env["LANGUAGE"]
    LC_CTYPE <- Sys.getlocale("LC_CTYPE")
    if (!is.na(LANG)) length(grep("^en", LANG, ignore.case=TRUE)) > 0
    else LC_CTYPE == "C" || length(grep("^en", LC_CTYPE, ignore.case=TRUE)) > 0
    }
    

# to replace tkmessageBox on non-English Windows systems,  
#  to allow for translation of button text

RcmdrTkmessageBox <- function(message, icon=c("info", "question", "warning",
    "error"), type=c("okcancel", "yesno", "ok"), default) {
    if ( (English()) || (.Platform$OS.type != "windows") ){
        if (missing(default)){
            default <- switch(type,
                okcancel="ok",
                yesno="yes",
                ok="ok")}
        return(tkmessageBox(message=message, icon=icon, type=type, 
            default=default))
        }
    icon <- match.arg(icon)
    type <- match.arg(type)
    initializeDialog(messageBox)
    messageFrame <- tkframe(messageBox, borderwidth=5)
    buttonFrame <- tkframe(messageBox,  borderwidth=5)
    if (icon != "question") tkbell()
    result <- tclVar()
    iconColor <- switch(icon, info="blue", question="blue", warning="black", 
        error="red")
    onOK <- function() {
        if (GrabFocus()) tkgrab.release(messageBox)
        tkdestroy(messageBox)  
        tkfocus(CommanderWindow())
        tclvalue(result) <- "ok"
        }
    OKbutton <- tkbutton(buttonFrame, text=gettextRcmdr("OK"), 
        fg="darkgreen", width="12", command=onOK, borderwidth=3,
        default=if (missing(default)) "active"
            else if (default == "ok") "active" else "normal")
    onCancel <- function() {
        if (GrabFocus()) tkgrab.release(messageBox)
        tkdestroy(messageBox)  
        tkfocus(CommanderWindow())
        tclvalue(result) <- "cancel"
        }
    cancelButton <- tkbutton(buttonFrame, text=gettextRcmdr("Cancel"), 
        fg="red", width="12", command=onCancel, borderwidth=3,
        default=if (missing(default)) "normal"
            else if (default == "cancel") "active" else "normal")           
    onYes <- function() {
        if (GrabFocus()) tkgrab.release(messageBox)
        tkdestroy(messageBox)  
        tkfocus(CommanderWindow())
        tclvalue(result) <- "yes"
        }
    yesButton <- tkbutton(buttonFrame, text=gettextRcmdr("Yes"), 
        fg="darkgreen", width="12", command=onYes, borderwidth=3,
        default=if (missing(default)) "active"
            else if (default == "yes") "active" else "normal")
    onNo <- function() {
        if (GrabFocus()) tkgrab.release(messageBox)
        tkdestroy(messageBox)  
        tkfocus(CommanderWindow())
        tclvalue(result) <- "no"
        }
    noButton <- tkbutton(buttonFrame, text=gettextRcmdr("No"), 
        fg="red", width="12", command=onNo, borderwidth=3,
        default=if (missing(default)) "normal"
            else if (default == "no") "active" else "normal")
        tkgrid(tklabel(messageFrame, bitmap=icon, fg=iconColor),
            tklabel(messageFrame, text="    "), 
            tklabel(messageFrame, text=message))
        tkgrid(messageFrame)        
    switch(type,
        okcancel = {
            tkgrid(OKbutton, tklabel(buttonFrame, text="    "), cancelButton)
            if (missing(default) || default == "ok") tkbind(messageBox, "<Return>",
                onOK)
            else if (default == "cancel") tkbind(messageBox, "<Return>", onCancel)
            },
        yesno =  {
            tkgrid(yesButton, tklabel(buttonFrame, text="    "), noButton)
            if (missing(default) || default == "yes") tkbind(messageBox, "<Return>",
                onYes)
            else if (default == "no") tkbind(messageBox, "<Return>", onNo)
            },
        ok = {
            tkgrid(OKbutton)
            if (missing(default) || default == "ok") tkbind(messageBox, "<Return>",
                onOK)
            }
        )
    tkgrid(buttonFrame)
    dialogSuffix(messageBox, rows=2, focus=messageBox, bindReturn=FALSE)
    result
    }