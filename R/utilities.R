# last modified 24 May 2003 by J. Fox

# utility functions

listDataSets <- function(envir=.GlobalEnv, ...) {
    names(which(sapply(ls(envir=envir, all.names=TRUE, ...), 
        function(x) is.data.frame(eval(parse(text=x))))))
    }

listLinearModels <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    objects[sapply(objects, 
        function(x) "lm" == (class(eval(parse(text=x)))[1]))]
    }

listGeneralizedLinearModels <- function(envir=.GlobalEnv, ...) {
    objects <- ls(envir=envir, ...)
    objects[sapply(objects, 
        function(x) "glm" == (class(eval(parse(text=x)))[1]))]
    }

activeDataSet <- function(dsname){
    if (missing(dsname)) {
        if (is.null(.activeDataSet)){
            tkmessageBox(message="There is no active data set.", icon="error", type="ok")
            return(FALSE)
            }
        else return(.activeDataSet)
        }
    if (!eval(parse(text=paste("is.data.frame(", dsname, ")", sep="")))){
        tkmessageBox(message=paste(dsname, " is not a data frame and cannot be attached.",
            sep=""), icon="error", type="ok")
        return()
        }
    if (!is.null(.activeDataSet)) {
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
    attach(eval(parse(text=dsname)), name=dsname)
    logger(paste("attach(", dsname, ")", sep=""))
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
    
listVariables <- function(dataSet=.activeDataSet) eval(parse(text=paste("names(", dataSet,")")))

listFactors <- function(dataSet=.activeDataSet) {
    variables <- listVariables(dataSet)
    variables[sapply(variables, function(x)
        is.factor(eval(parse(text=x), envir=eval(parse(text=dataSet)))))]
    }

listTwoLevelFactors <- function(dataSet=.activeDataSet){
    factors <- listFactors(dataSet)
    if(length(factors) == 0) return(NULL)
    factors[sapply(factors, function(x)
        2 == length(levels(eval(parse(text=x), envir=eval(parse(text=dataSet))))))]
    }
    
listNumeric <- function(dataSet=.activeDataSet) {
    variables <- listVariables(dataSet)
    variables[sapply(variables,function(x)
        is.numeric(eval(parse(text=x), envir=eval(parse(text=dataSet)))))]
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

# the following function from Fox, An R and S-PLUS Companion to Applied Regression

influence.plot <- function(model, scale=10, col=c(1,2),
    labels=names(rstud), ...){
    hatval <- hatvalues(model)
    rstud <- rstudent(model)
    cook <- sqrt(cookd(model))
    scale <- scale/max(cook)
    p <- length(coef(model))
    n <- length(rstud)
    cutoff <- sqrt(4/(n - p))
    plot(hatval, rstud, xlab='Hat-Values',
        ylab='Studentized Residuals', type='n', ...)
    abline(v=c(2, 3)*p/n, lty=2)
    abline(h=c(-2, 0, 2), lty=2)
    for (i in 1:n) 
        points(hatval[i], rstud[i], cex=scale*cook[i], 
            col=if (cook[i] > cutoff) col[2] else col[1])
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
    rownames(rel) <- as.character(1:k)
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
