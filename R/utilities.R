# last modified 22 July 2003 by J. Fox

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
    
listVariables <- function(dataSet=.activeDataSet) eval(parse(text=paste("names(", dataSet,")")),
    envir=.GlobalEnv)

listFactors <- function(dataSet=.activeDataSet) {
    variables <- listVariables(dataSet)
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
    variables <- listVariables(dataSet)
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
    
colPercents <- function(tab){
    dim <- length(dim(tab))
    sums <- apply(tab, 2:dim, sum)
    per <- apply(tab, 1, function(x) x/sums)
    dim(per) <- dim(tab)[c(2:dim,1)]
    per <- aperm(per, c(dim, 1:(dim-1)))
    dimnames(per) <- dimnames(tab)
    100*per
    }


rowPercents <- function(tab){
    dim <- length(dim(tab))
    if (dim == 2) return(t(colPercents(t(tab))))
    tab <- aperm(tab, c(2,1,3:dim))
    aperm(colPercents(tab), c(2,1,3:dim))
    }

# the following function slightly modified from Brian Ripley via R-help

levene.test <- function(y, group) {
    meds <- tapply(y, group, median, na.rm=TRUE)
    resp <- abs(y - meds[group])
    table <- anova(lm(resp ~ group))
    rownames(table)[2] <- " "
    cat("Levene's Test for Homogeneity of Variance\n\n")
    table[,c(1,4,5)]
    } 

# the following function adapted from Fox, An R and S-PLUS Companion to Applied Regression

influence.plot <- function(model, scale=10, col=c(1,2),
    labels=names(rstud), ...){
    hatval <- hatvalues(model)
    rstud <- rstudent(model)
    cook <- sqrt(cookd(model))
    scale <- scale/max(cook, na.rm=TRUE)
    p <- length(coef(model))
    n <- length(rstud)
    cutoff <- sqrt(4/(n - p))
    plot(hatval, rstud, xlab='Hat-Values',
        ylab='Studentized Residuals', type='n', ...)
    abline(v=c(2, 3)*p/n, lty=2)
    abline(h=c(-2, 0, 2), lty=2)
    points(hatval, rstud, cex=scale*cook, 
            col=ifelse(cook > cutoff, col[2], col[1]))
    if (labels[1] != FALSE) identify(hatval, rstud, labels)
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
    k <- dim(S)[1]
    s <- sqrt(diag(S))
    R <- S/(s %o% s)
    rel <- reliab(S, R)
    cat(paste("Alpha reliability = ", round(rel[1], 4), "\n"))
    cat(paste("Standardized alpha = ", round(rel[2], 4), "\n"))
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
    round(rel, 4)
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
