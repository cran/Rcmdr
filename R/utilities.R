# last modified 18 Nov 04 by J. Fox + slight changes 12 Aug 04 by Ph. Grosjean

# utility functions

listDataSets <- function(envir=.GlobalEnv, ...) {
    names(which(sapply(ls(envir=envir, all.names=TRUE, ...), 
        function(.x) is.data.frame(eval(parse(text=.x), envir=envir)))))
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
        function(.x) (class(eval(parse(text=.x), envir=envir))[1])) %in% .modelClasses]
    }
                
activeDataSet <- function(dsname){
    if (missing(dsname)) {
        if (is.null(.activeDataSet)){
            tkmessageBox(message="There is no active data set.", icon="error", type="ok")
            return(FALSE)
            }
        else return(.activeDataSet)
        }
    if (!is.data.frame(get(dsname, envir=.GlobalEnv))){
        tkmessageBox(message=paste(dsname, " is not a data frame and cannot be attached.",
            sep=""), icon="error", type="ok")
        tkfocus(.commander)
        return()
        }
    if (!is.null(.activeDataSet) && .attach.data.set
        && (length(grep(.activeDataSet, search())) !=0)) {
        detach(pos = match(.activeDataSet, search()))
        logger(paste("detach(", .activeDataSet, ")", sep=""))
        }
    assign(".activeModel", NULL, envir=.GlobalEnv)
    tclvalue(.modelName) <- "<No active model>"
    # -PhG tkconfigure(.modelLabel, fg="red")
    assign(".activeDataSet", dsname, envir=.GlobalEnv)
    assign(".variables", listVariables(), envir=.GlobalEnv)
    assign(".numeric", listNumeric(), envir=.GlobalEnv)
    assign(".factors", listFactors(), envir=.GlobalEnv)
    assign(".twoLevelFactors", listTwoLevelFactors(), envir=.GlobalEnv)
    tclvalue(.dataSetName) <- paste(.activeDataSet, " ")
    # -PhG tkconfigure(.dataSetLabel, fg="blue")
    if (!is.SciViews()) tkconfigure(.dataSetLabel, fg="blue") else refreshStatus() # +PhG
    if (.attach.data.set){
        attach(get(dsname, envir=.GlobalEnv), name=dsname)
        logger(paste("attach(", dsname, ")", sep=""))
        }
    if (!is.SciViews()) tkconfigure(.modelLabel, fg="red") else refreshStatus() # +PhG
    dsname
    }


activeModel <- function(model){
    if (missing(model)) {
        if (is.null(.activeModel)){
            tkmessageBox(message="There is no active model.", icon="error", type="ok")
            return(FALSE)
            }
        else return(.activeModel)
        }
    assign(".activeModel", model, envir=.GlobalEnv)
    tclvalue(.modelName) <- .activeModel
    # -PhG tkconfigure(.modelLabel, fg="blue")
    if (!is.SciViews()) tkconfigure(.modelLabel, fg="blue") else refreshStatus() # +PhG
    model
    }
    
listVariables <- function(dataSet=.activeDataSet) {
    vars <- eval(parse(text=paste("names(", dataSet,")")), envir=.GlobalEnv)
    if (.sort.names) sort(vars) else vars
    }

listFactors <- function(dataSet=.activeDataSet) {
    variables <- if (exists(".variables")) .variables else listVariables(dataSet)
    variables[sapply(variables, function(.x)
        is.factor(eval(parse(text=.x), envir=eval(parse(text=dataSet), envir=.GlobalEnv))))]
    }

listTwoLevelFactors <- function(dataSet=.activeDataSet){
    factors <- listFactors(dataSet)
    if(length(factors) == 0) return(NULL)
    factors[sapply(factors, function(.x)
        2 == length(levels(eval(parse(text=.x), envir=eval(parse(text=dataSet), 
            envir=.GlobalEnv)))))]
    }
    
listNumeric <- function(dataSet=.activeDataSet) {
    variables <- if (exists(".variables")) .variables else listVariables(dataSet)
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
    
colPercents <- function(tab, digits=2){
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

rowPercents <- function(tab, digits=2){
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
        stop("argument must be a square, symmetric, numeric covariance matrix")
    k <- dim(S)[1]
    s <- sqrt(diag(S))
    R <- S/(s %o% s)
    rel <- reliab(S, R)
    result$alpha <- rel[1]
    result$st.alpha <- rel[2]
    if (k < 3) {
        warning("there are fewer than 3 items in the scale")
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
    if (!is.numeric(response)) stop("Argument response must be numeric.")
    xlab # force evaluation
    ylab
    legend.lab
    error.bars <- match.arg(error.bars)
    if (missing(factor2)){
        if (!is.factor(factor1)) stop("Argument factor1 must be a factor.")
        valid <- !(is.na(factor1) | is.na(response))
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
        if (!(is.factor(factor1) | is.factor(factor2))) stop("Arguments factor1 and factor2 must be factors.")
        valid <- !(is.na(factor1) | is.na(factor2) | is.na(response))
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
        if (n.levs.2 > length(col)) stop(paste("Number of groups for factor2, ", n.levs.2,
            ", exceeds number of distinct colours, ", length(col), ".", sep=""))
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
# Author: Dan Putler (revision by J. Fox, 27 July 04)
    if(length(x) < bins) {
      stop("The number of bins exceeds the number of data values")
        }
    x <- if(method == "intervals") cut(x, bins, labels=labels)
        else if (method == "proportions") cut(x, quantile(x, probs=seq(0,1,1/bins), na.rm=TRUE),
            include.lowest = TRUE, labels=labels)
        else {
            xx <- na.omit(x)
            breaks <- c(min(xx), tapply(xx, kmeans(xx, bins)$cluster, max))
            cut(x, breaks, include.lowest=TRUE, labels=labels)
            }
    as.factor(x)
    }
    
# 3D scatterplots via rgl

scatter3d <- function(x, y, z, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
                      zlab=deparse(substitute(z)), revolutions=0, bg.col=c("black", "white"), axis.col=NULL,
                      surface.col=c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"), 
                      neg.res.col="red", pos.res.col="green", point.col="yellow",
                      text.col=axis.col, grid.col=if (bg.col == "white") "black" else "gray", 
                      fogtype=c("exp2", "linear", "exp", "none"), 
                      residuals=(length(fit) == 1), surface=TRUE, grid=TRUE, df.smooth=NULL, df.additive=NULL,
                      sphere.size=1, threshold=0.01, speed=1, fov=60, 
                      fit="linear", groups=NULL, parallel=TRUE, model.summary=FALSE){
    require(rgl)
    require(mgcv)
    summaries <- list()
    if ((!is.null(groups)) && (nlevels(groups) > length(surface.col))) stop(paste("Number of groups (", 
        nlevels(groups), ") exceeds number of colors (", length(surface.col), ").", sep=""))
    if ((!is.null(groups)) && (!is.factor(groups))) stop("groups variable must be a factor.")
    bg.col <- match.arg(bg.col)
    fogtype <- match.arg(fogtype)
    if ((length(fit) > 1) && residuals && surface)
        stop("cannot plot both multiple surfaces and residuals")
    if (is.null(axis.col)) axis.col <- if (bg.col == "white") "black" else "white"
    xlab
    ylab
    zlab
    rgl.clear()
    rgl.viewpoint(fov=fov)
    rgl.bg(col=bg.col, fogtype=fogtype)
    valid <- if (is.null(groups)) !(is.na(x) | is.na(y) | is.na(z))
        else !(is.na(x) | is.na(y) | is.na(z) | is.na(groups))
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
    rgl.texts(1, 0, 0, xlab, justify="right", color=text.col)
    rgl.texts(0, 1, 0, ylab, justify="right", color=text.col)
    rgl.texts(0, 0, 1, zlab, justify="right", color=text.col)
    if (surface){
        for (i in 1:length(fit)){
            f <- match.arg(fit[i], c("linear", "quadratic", "smooth", "additive"))
            vals <- seq(0, 1, length=26)
            dat <- expand.grid(x=vals, z=vals)
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
                yhat <- matrix(predict(mod, newdata=dat), 26, 26)
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
                        yhat <- matrix(predict(mod, newdata=cbind(dat, groups=group)), 26, 26)
                        rgl.surface(vals, vals, yhat, color=surface.col[j], alpha=0.5, lit=FALSE)
                        if (grid) rgl.surface(vals, vals, yhat, color=grid.col, alpha=0.5, lit=FALSE, front="lines", back="lines")
                        rgl.texts(0, predict(mod, newdata=data.frame(x=0, z=0, groups=group)), 0, 
                            paste(group, " "), justify="right", color=surface.col[j])
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
                        yhat <- matrix(predict(mod, newdata=dat), 26, 26)
                        rgl.surface(vals, vals, yhat, color=surface.col[j], alpha=0.5, lit=FALSE)
                        rgl.surface(vals, vals, yhat, color=grid.col, alpha=0.5, lit=FALSE, front="lines", back="lines")
                        rgl.texts(0, predict(mod, newdata=data.frame(x=0, z=0, groups=group)), 0, 
                            paste(group, " "), justify="right", color=surface.col[j])
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
        txt <- tktext(tt, bg = "white", font = .logFont)
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
    if (as.numeric(R.Version()$major) >= 2) print(help("aboutRcmdr"))
    else help("aboutRcmdr")
    }

browseManual <- function() {
    browseURL(paste(file.path(.path.package(package="Rcmdr")[1], "doc"), 
        "/Getting-Started-with-the-Rcmdr.pdf", sep=""))
    }

printRcmdrDataSetHelp <- function (x, ...) 
## this is slightly modified from print.help_files_with_topic in the utils package in R 2.0.0
## replacing calls to writeLines() with calls to warning()
{
    topic <- attr(x, "topic")
    paths <- as.character(x)
    if (!length(paths)) {
        warning(c(paste("No documentation for", sQuote(topic), 
            "in specified packages and libraries:"), paste("you could try", 
            sQuote(paste("help.search(", dQuote(topic), ")", 
                sep = "")))))
        return(invisible(x))
    }
    if (attr(x, "tried_all_packages")) {
        paths <- unique(dirname(dirname(paths)))
        msg <- paste("Help for topic", sQuote(topic), "is not in any loaded package but can be found", 
            "in the following packages:")
        warning(c(strwrap(msg), "", paste(" ", formatDL(c("Package", 
            basename(paths)), c("Library", dirname(paths)), indent = 22))))
    }
    else {
        if (length(paths) > 1) {
            file <- paths[1]
            msg <- paste("Help on topic", sQuote(topic), "was found in the following packages:")
            paths <- dirname(dirname(paths))
            warning(c(strwrap(msg), "", paste(" ", formatDL(c("Package", 
                basename(paths)), c("Library", dirname(paths)), 
                indent = 22)), "\nUsing the first match ..."))
        }
        else file <- paths
        type <- attr(x, "type")
        if (type == "html") {
            if (file.exists(file)) 
                .show_help_on_topic_as_HTML(file, topic)
            else stop(paste("No HTML help for ", sQuote(topic), 
                " is available:\n", "corresponding file is missing.", 
                sep = ""))
        }
        else if (type == "chm") {
            chm.dll <- file.path(R.home(), "bin", "Rchtml.dll")
            if (!file.exists(chm.dll)) 
                stop("Compiled HTML is not installed")
            if (!is.loaded(symbol.C("Rchtml"))) 
                dyn.load(chm.dll)
            wfile <- sub("/chm/([^/]*)$", "", file)
            thispkg <- sub(".*/([^/]*)/chm/([^/]*)$", "\\1", 
                file)
            thispkg <- sub("_.*$", "", thispkg)
            hlpfile <- paste(wfile, "/chtml/", thispkg, ".chm", 
                sep = "")
            if (file.exists(hlpfile)) {
                err <- .C("Rchtml", hlpfile, topic, err = integer(1), 
                  PACKAGE = "")$err
                if (err) 
                  stop("CHM file could not be displayed")
            }
            else stop(paste("No CHM help for ", sQuote(topic), 
                " is available:\n", "corresponding file is missing.", 
                sep = ""))
        }
        else if (type == "help") {
            zfile <- zip.file.extract(file, "Rhelp.zip")
            if (file.exists(zfile)) 
                file.show(zfile, title = paste("R Help on", sQuote(topic)), 
                  delete.file = (zfile != file), pager = attr(x, 
                    "pager"))
            else stop(paste("No text help for", sQuote(topic), 
                " is available:\n", "corresponding file is missing.", 
                sep = ""))
        }
        else if (type == "latex") {
            ok <- FALSE
            zfile <- zip.file.extract(file, "Rhelp.zip")
            if (zfile != file) 
                on.exit(unlink(zfile))
            if (file.exists(zfile)) {
                .show_help_on_topic_offline(zfile, topic)
                ok <- TRUE
            }
            else if (interactive()) {
                path <- dirname(file)
                dirpath <- dirname(path)
                pkgname <- basename(dirpath)
                Rdpath <- file.path(dirpath, "man", paste(pkgname, 
                  "Rd.gz", sep = "."))
                if (file.exists(Rdpath)) {
                  ans <- readline("No latex file is available: shall I try to create it? (y/n) ")
                  if (substr(ans, 1, 1) == "y") {
                    lines <- tools:::extract_Rd_file(Rdpath, 
                      topic)
                    tf <- tempfile("Rd")
                    tf2 <- tempfile("Rlatex")
                    writeLines(lines, tf)
                    cmd <- paste("R CMD Rdconv -t latex", tf, 
                      ">", tf2)
                    res <- system(cmd)
                    if (res) 
                      stop("problems running R CMD Rdconv")
                    .show_help_on_topic_offline(tf2, topic)
                    ok <- TRUE
                  }
                }
            }
            if (!ok) 
                stop(paste("No offline help for ", sQuote(topic), 
                  " is available:\n", "corresponding file is missing.", 
                  sep = ""))
        }
    }
    invisible(x)
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
            msg <- paste(a[[i]], "not supplied")
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
        OKbutton <- tkbutton(buttonsFrame, text="OK", fg="darkgreen", width="12", command=onOK, default="active",
            borderwidth=3)
        onCancel <- function() {
            if (model) assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv)               
            if (.grab.focus) tkgrab.release(window)
            tkdestroy(window)  
            tkfocus(.commander)
            }
        cancelButton <- tkbutton(buttonsFrame, text="Cancel", fg="red", width="12", command=onCancel, borderwidth=3)
        if (!is.null(helpSubject)){
            onHelp <- function() {
                if (.grab.focus && .Platform$OS.type != "windows") tkgrab.release(window)
                if (as.numeric(R.Version()$major) >= 2) print(help(helpSubject))
                else help(helpSubject)
                }
            helpButton <- tkbutton(buttonsFrame, text="Help", width="12", command=onHelp, borderwidth=3)
            }       
        tkgrid(OKbutton, tklabel(buttonsFrame, text="  "), cancelButton, tklabel(buttonsFrame, text="            "), 
            if (!is.null(helpSubject)) helpButton, sticky="w")
        })

subOKCancelHelp <- defmacro(window=subdialog, helpSubject=NULL,
    expr={
        subButtonsFrame <- tkframe(window, borderwidth=5)
        subOKbutton <- tkbutton(subButtonsFrame, text="OK", fg="darkgreen", width="12", command=onOKsub, default="active",
            borderwidth=3)
        onCancelSub <- function() {
            if (.grab.focus) tkgrab.release(window)
            tkdestroy(window)  
            tkfocus(.commander)
            }
        subCancelButton <- tkbutton(subButtonsFrame, text="Cancel", fg="red", width="12", command=onCancelSub, 
            borderwidth=3)
        if (!is.null(helpSubject)){
            onHelpSub <- function(){
                if (.grab.focus && .Platform$OS.type != "windows") tkgrab.release(window)
                if (as.numeric(R.Version()$major) >= 2) print(help(helpSubject))
                else help(helpSubject)
                }
            subHelpButton <- tkbutton(subButtonsFrame, text="Help", width="12", command=onHelpSub, borderwidth=3)
            }       
        tkgrid(subOKbutton, tklabel(subButtonsFrame, text="  "), subCancelButton, 
            tklabel(subButtonsFrame, text="            "), if (!is.null(helpSubject)) subHelpButton, sticky="w")
        })

checkActiveDataSet <- function(){
    if (activeDataSet() == FALSE) {
        tkfocus(.commander)
        FALSE
        }
    else TRUE
    }
    
checkActiveModel <- function(){
    if (activeModel() == FALSE) {
        tkfocus(.commander)
        FALSE
        }
    else TRUE
    }
    
checkFactors <- function(n=1){
    if (length(.factors) < n){
        if (n > 1)
            tkmessageBox(message=paste("There fewer than", n, "factors in the active data set."), 
                    icon="error", type="ok")
        else tkmessageBox(message="There are no factors in the active data set.", 
                    icon="error", type="ok")
        tkfocus(.commander)
        FALSE
        }
    else TRUE
    }
    
checkTwoLevelFactors <- function(n=1){
    if (length(.twoLevelFactors) < n){
        if (n > 1)
            tkmessageBox(message=paste("There fewer than", n, "two-level factors in the active data set."), 
                    icon="error", type="ok")
        else tkmessageBox(message="There are no two-level factors in the active data set.", 
                    icon="error", type="ok")
        tkfocus(.commander)
        FALSE
        }
    else TRUE
    }
    
checkNumeric <- function(n=1){
    if (length(.numeric) < n){
        if (n > 1)
            tkmessageBox(message=paste("There fewer than", n, "numeric variables in the active data set."), 
                    icon="error", type="ok")
        else tkmessageBox(message="There are no numeric variables in the active data set.", 
                    icon="error", type="ok")
        tkfocus(.commander)
        FALSE
        }
    else TRUE
    }
    
checkVariables <- function(n=1){
    if (length(.variables) < n){
        if (n > 1)
            tkmessageBox(message=paste("There fewer than", n, "variables in the active data set."), 
                    icon="error", type="ok")
        else tkmessageBox(message="There are no variables in the active data set.", 
                    icon="error", type="ok")
        tkfocus(.commander)
        FALSE
        }
    else TRUE
    }

initializeDialog <- defmacro(window=top, title="", 
    expr={
        window <- tktoplevel(borderwidth=10)
        tkwm.title(window, title)
        }
    )

dialogSuffix <- defmacro(window=top, onOK=onOK, rows=1, columns=1, focus=top,
    bindReturn=TRUE, preventGrabFocus=FALSE,
    expr={
        for (row in 0:(rows-1)) tkgrid.rowconfigure(window, row, weight=0)
        for (col in 0:(columns-1)) tkgrid.columnconfigure(window, col, weight=0)
        .Tcl("update idletasks")
        tkwm.resizable(window, 0, 0)
        if (bindReturn) tkbind(window, "<Return>", onOK)
        if (.double.click) tkbind(window, "<Double-ButtonPress-1>", onOK)
        tkwm.deiconify(window)
        # focus grabs appear to cause problems for some dialogs
        if (.grab.focus && (!preventGrabFocus)) tkgrab.set(window)
        tkfocus(focus)
        tkwait.window(window)
        }
    )

            
variableListBox <- function(parentWindow, variableList=.variables, bg="white",
    selectmode="single", export="FALSE", initialSelection=NULL, title){
    if (selectmode == "multiple") selectmode <- .multiple.select.mode
    frame <- tkframe(parentWindow)
    listbox <- tklistbox(frame, height=min(4, length(variableList)),
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

checkReplace <- function(name, type="Variable"){
    tkmessageBox(message=paste(type, " ", name, " already exists.\nOverwrite ", 
        tolower(type),"?", sep=""), icon="warning", type="yesno", default="no")
    }

errorCondition <- defmacro(window=top, recall=NULL, message, model=FALSE,
    expr={
        if (model) assign(".modelNumber", .modelNumber - 1, envir=.GlobalEnv) 
        if (.grab.focus) tkgrab.release(window)
        tkdestroy(window)
        tkmessageBox(message=message,
            icon="error", type="ok", default="ok")
        if (!is.null(recall)) recall()
        })

subsetBox <- defmacro(window=top, model=FALSE, 
    expr={
            subsetVariable <- if (model){
                if (currentModel && currentFields$subset != "") 
                    tclVar(currentFields$subset) else tclVar("<all valid cases>")
                }
            else tclVar("<all valid cases>")
            subsetFrame <- tkframe(window)
            subsetEntry <- tkentry(subsetFrame, width="20", textvariable=subsetVariable)
            subsetScroll <- tkscrollbar(subsetFrame, orient="horizontal",
                repeatinterval=5, command=function(...) tkxview(subsetEntry, ...))
            tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
            tkgrid(tklabel(subsetFrame, text="Subset expression", fg="blue"), sticky="w")
            tkgrid(subsetEntry, sticky="w")
            tkgrid(subsetScroll, sticky="ew")
            })

groupsBox <- defmacro(recall=NULL, label="Plot by:", initialLabel="Plot by groups",
    plotLinesByGroup=FALSE, positionLegend=FALSE, plotLinesByGroupsText="Plot lines by group",
    expr={
        env <- environment()
        .groups <- FALSE
        .linesByGroup <- FALSE
        .groupsLabel <- tclVar(paste(initialLabel, "...", sep=""))
        onGroups <- function(){
            if (length(.factors) == 0){
                errorCondition(recall=recall, message="There no factors in the active data set.") 
                return()
                }
            initializeDialog(subdialog, title="Groups")
            groupsBox <- variableListBox(subdialog, .factors, title="Groups variable (pick one)")
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
                    if (.grab.focus) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    tkwm.deiconify(top)
                    if (.grab.focus) tkgrab.set(top)
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
                if (.grab.focus) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                tkwm.deiconify(top)
                if (.grab.focus) tkgrab.set(top)
                tkfocus(top)
                tkwait.window(top)
                }
            subOKCancelHelp()
            tkgrid(getFrame(groupsBox), sticky="nw")
            if (plotLinesByGroup) tkgrid(linesByGroupFrame, sticky="w")
            tkgrid(subButtonsFrame, sticky="w")
            if (positionLegend) tkgrid(tklabel(subdialog, text="Position legend with mouse click", fg="blue"))
            dialogSuffix(subdialog, onOK=onOKsub, rows=3+plotLinesByGroup+positionLegend, columns=2, focus=subdialog)
            }
        groupsFrame <- tkframe(top)
        groupsButton <- tkbutton(groupsFrame, textvariable=.groupsLabel, command=onGroups, borderwidth=3)
        tkgrid(tklabel(groupsFrame, text="    "), groupsButton, sticky="w")
        })

groupsLabel <- defmacro(frame=top, groupsBox=groupsBox, columnspan=1,
    expr={
        groupsFrame <- tkframe(frame)
        groupsLabel <- tklabel(groupsFrame, text="<No groups selected>")    
        tkgrid(tklabel(groupsFrame, text="Difference: ", fg="blue"), groupsLabel, sticky="w")
        tkgrid(groupsFrame, sticky="w", columnspan=columnspan)
        onSelect <- function(){
            group <- getSelection(groupsBox)
            levels <- eval(parse(text=paste("levels(", .activeDataSet, "$", group, ")", sep="")))
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
    variables <- paste(.variables, ifelse(is.element(.variables, .factors), "[factor]", ""))
    xBox <- variableListBox(frame, variables, title="Variables (double-click to formula)")
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
        tkgrid(tklabel(outerOperatorsFrame, text="Model Formula:     ", fg="blue"), operatorsFrame)
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

checkMethod <- defmacro(generic, object, message=NULL, default=FALSE, strict=FALSE,
    expr={
        msg <- if (is.null(message)) paste("No appropriate", generic, "method exists\nfor a model of this class.")
            else message
        method <- exists.method(generic, eval(parse(text=object)), default=default, strict=strict)
        if (!method) tkmessageBox(message=msg, icon="error", type="ok", default="ok")
        method
        }
    )
    
checkClass <- defmacro(object, class, message=NULL,
    expr={
        msg <- if (is.null(message)) paste('The model is not of class "', class, '".')
            else message
       properClass <- eval(parse(text=paste("class(", object, ")")))[1] == class
       if (!properClass) tkmessageBox(message=msg, icon="error", type="ok", default="ok")
       properClass
       }
    )
