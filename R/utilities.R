# last modified 5 May 04 by J. Fox

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
    if (!is.null(.activeDataSet) && (tclvalue(.attachDataSet) == "1") 
        && (length(grep(.activeDataSet, search())) !=0)) {
        detach(pos = match(.activeDataSet, search()))
        logger(paste("detach(", .activeDataSet, ")", sep=""))
        }
    assign(".activeModel", NULL, envir=.GlobalEnv)
    tclvalue(.modelName) <- "<No active model>"
    tkconfigure(.modelLabel, fg="red")
    assign(".activeDataSet", dsname, envir=.GlobalEnv)
    assign(".variables", listVariables(), envir=.GlobalEnv)
    assign(".numeric", listNumeric(), envir=.GlobalEnv)
    assign(".factors", listFactors(), envir=.GlobalEnv)
    assign(".twoLevelFactors", listTwoLevelFactors(), envir=.GlobalEnv)
    tclvalue(.dataSetName) <- paste(.activeDataSet, " ")
    tkconfigure(.dataSetLabel, fg="blue")
    if (tclvalue(.attachDataSet) == "1"){
        attach(get(dsname, envir=.GlobalEnv), name=dsname)
        logger(paste("attach(", dsname, ")", sep=""))
        }
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
    tkconfigure(.modelLabel, fg="blue")
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
    if ((!is.numeric(S)) || !is.matrix(S) || (nrow(S) != ncol(S)) 
        || any(abs(S - t(S)) > max(abs(S))*1e-10) || nrow(S) < 2)
        stop("argument must be a square, symmetric, numeric covariance matrix")
    k <- dim(S)[1]
    s <- sqrt(diag(S))
    R <- S/(s %o% s)
    rel <- reliab(S, R)
    cat(paste("Alpha reliability = ", round(rel[1], 4), "\n"))
    cat(paste("Standardized alpha = ", round(rel[2], 4), "\n"))
    if (k < 3) {
        warning("there are fewer than 3 items in the scale")
        return(invisible(NULL))
        }
    cat("\nReliability deleting each item in turn:\n")
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
    print(round(rel, 4))
    invisible(NULL)
    }
    
partial.cor <- function(X, ...){
    R <- cor(X, ...)
    RI <- solve(R)
    D <- diag(1/sqrt(diag(RI)))
    R <- -D %*% RI %*% D
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
     style=c("Tukey", "bare"), trim.outliers=TRUE, depths=TRUE, reverse.negative.leaves=TRUE,
     print=TRUE){
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
    if (print){
        for(i in seq(result)) cat(result[[i]],sep="\n")
        invisible(NULL)
        }
    else result
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
                if (model.summary) print(summary(mod))
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
                    if (model.summary) print(summary(mod))
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
                        if (model.summary) print(summary(mod))
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

    # Open browser with manual
    
browseManual <- function() {
    browseURL(paste(file.path(.path.package(package="Rcmdr")[1], "doc"), 
        "/Getting-Started-with-the-Rcmdr.pdf", sep=""))
    }
