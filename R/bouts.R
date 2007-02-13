## $Id: bouts.R,v 1.7 2007-02-13 17:22:53 sluque Exp $

"logit" <- function(p) log(p / (1 - p))

"unLogit" <- function(logit) exp(logit) / (exp(logit) + 1)

"boutfreqs" <- function(x, bw, method=c("standard", "seq.diff"), plot=TRUE)
{
    ## Value: data frame with log frequencies and bin mid-points
    ## --------------------------------------------------------------------
    ## Arguments: x=numeric vector, bw=bin width for histogram,
    ## plot=logical whether to plot or not
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    method <- match.arg(method)
    switch(method,
           standard = {upper <- max(x, na.rm=TRUE)
                       brks <- seq(min(x, na.rm=TRUE), upper, bw)
                       if (brks[length(brks)] < upper) {
                           brks <- c(brks, brks[length(brks)] + bw)
                       }
                       h <- hist(x, br=brks, include.lowest=TRUE, plot=plot)},
           seq.diff = {diff.x <- abs(diff(x))
                       upper <- max(diff.x, na.rm=TRUE)
                       brks <- seq(0, upper, bw)
                       if (brks[length(brks)] < upper) {
                           brks <- c(brks, brks[length(brks)] + bw)
                       }
                       h <- hist(diff.x, br=brks, include.lowest=TRUE,
                                 plot=plot)})
    ok <- which(h$counts > 0)
    freq.adj <- h$counts[ok] / diff(c(0, ok))
    data.frame(lnfreq=log(freq.adj), x=h$mids[ok])
}

"boutinit" <- function(lnfreq, x.break, plot=TRUE)
{
    ## Value: list with starting values for nls bout function
    ## --------------------------------------------------------------------
    ## Arguments: lnfreq=data frame with 'lnfreq' (log frequencies) and
    ## 'x' (midpoints), x.break=x value defining the break point for
    ## broken stick model, such that x < x.break is 1st process, and x >=
    ## x.break is 2nd one, plot=logical whether to plot or not
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    proc1 <- lnfreq$x < x.break
    proc2 <- lnfreq$x >= x.break
    bkstick1 <- coef(lm(lnfreq ~ x, lnfreq, subset=proc1))
    bkstick2 <- coef(lm(lnfreq ~ x, lnfreq, subset=proc2))
    lambda1 <- as.vector(-bkstick1[2])
    a1 <- as.vector(exp(bkstick1[1]) / lambda1)
    lambda2 <- as.vector(-bkstick2[2])
    a2 <- as.vector(exp(bkstick2[1]) / lambda2)
    if (plot) {
        plot(lnfreq ~ x, lnfreq, las=1, type="n")
        points(lnfreq[proc1] ~ x[proc1], lnfreq, pch=21, bg="white")
        points(lnfreq[proc2] ~ x[proc2], lnfreq, pch=21, bg="black")
        curve(log(a1 * lambda1 * exp(-lambda1 * x) +
                  a2 * lambda2 * exp(-lambda2 * x)),
              min(lnfreq$x), max(lnfreq$x), add=TRUE)
        abline(bkstick1, lty=2)
        abline(bkstick2, lty=3)
    }
    list(a1=a1, lambda1=lambda1, a2=a2, lambda2=lambda2)
}

"bouts2.nlsFUN" <- function(x, a1, lambda1, a2, lambda2) {
    log(a1 * lambda1 * exp(-lambda1 * x) + a2 * lambda2 * exp(-lambda2 * x))
}

"bouts2.nls" <- function(lnfreq, start, maxiter)
{
    ## Value: list with non linear fitted model and bout ending criterion
    ## --------------------------------------------------------------------
    ## Arguments: lnfreq=data frame with 'lnfreq' (log frequencies) and 'x'
    ## (midpoints), start, maxiter=arguments for nls.
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    fit.nls <- nls(lnfreq ~ bouts2.nlsFUN(x, a1, lambda1, a2, lambda2),
                   data=lnfreq, start=start,
                   control=nls.control(maxiter=maxiter))
    fit.nls
}

"bouts2.nlsBEC" <- function(fit)
{
    ## Value: Numeric with bout ending criterion
    ## --------------------------------------------------------------------
    ## Arguments: list with nls fit
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    coefs <- coef(fit)
    if (length(coefs) != 4) {
        stop("fit must have 4 coefficients in a 2-process model")
    }
    a1_hat <- as.vector(coefs[1])
    lambda1_hat <- as.vector(coefs[2])
    a2_hat <- as.vector(coefs[3])
    lambda2_hat <- as.vector(coefs[4])
    log((a1_hat * lambda1_hat) / (a2_hat * lambda2_hat)) /
        (lambda1_hat - lambda2_hat)
}

"plotBouts2.nls" <- function(fit, lnfreq, bec.lty=2, ...)
{
    ## Value: plot of fitted model of log frequencies on x, with bec line.
    ## --------------------------------------------------------------------
    ## Arguments: fit=nls list, lnfreq=data frame with named objects
    ## lnfreq and x.
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    coefs <- coef(fit)
    bec <- bouts2.nlsBEC(fit)
    a1_hat <- as.vector(coefs[1])
    lambda1_hat <- as.vector(coefs[2])
    a2_hat <- as.vector(coefs[3])
    lambda2_hat <- as.vector(coefs[4])
    plot(lnfreq ~ x, lnfreq, las=1, type="n", ...)
    curve(log(a1_hat * lambda1_hat * exp(-lambda1_hat * x) +
              a2_hat * lambda2_hat * exp(-lambda2_hat * x)),
          min(lnfreq$x), max(lnfreq$x), add=TRUE)
    points(lnfreq ~ x, lnfreq, pch=21, bg="white")
    becy <- predict(fit, list(x=bec))
    usr <- par("usr")
    arrows(bec, becy, bec, usr[3], code=0, lty=bec.lty)
    legend(bec, usr[3] + ((usr[4] - usr[3]) * 0.08),
           paste("bec = ", round(bec, 2), sep=""), bty="n", cex=0.8)
    a1_hat <- round(a1_hat, 2)
    a2_hat <- round(a2_hat, 3)
    lambda1_hat <- round(lambda1_hat, 3)
    lambda2_hat <- round(lambda2_hat, 4)
    legend("topright",
           legend=bquote(y == log(.(a1_hat) %.% .(lambda1_hat) %.%
                             e^(- .(lambda1_hat) * x) +
                             .(a2_hat) %.% .(lambda2_hat) %.%
                             e^(- .(lambda2_hat) * x))),
           bty="n", cex=0.8, adj=c(0, 1))
}

"labelBouts" <- function(x, bec, bec.method=c("standard", "seq.diff"))
{
    ## Value: a numeric vector labelling each row in x with a unique,
    ## sequential bout number
    ## --------------------------------------------------------------------
    ## Arguments: x=numeric vector or matrix with variable or variables,
    ## respectively, to use for splitting bouts; bec=vector or matrix with
    ## corresponding bout ending criterion (i.e. each element/column of x
    ## is compared against the element in bec at the same index).
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    if (!is(x, "matrix")) x <- as.matrix(x)
    if (!is(bec, "matrix")) bec <- as.matrix(bec)
    if (!identical(dim(x), dim(bec)))
        stop(paste("x and bec must have the same", "dimensions"))
    bec.method <- match.arg(bec.method)
    switch(bec.method,
           standard = {xx <- x[-1, ]},
           seq.diff = {xx <- apply(x, 2, function(k) abs(diff(k)))})
    testfun <- function(xi, beci) ifelse(xi > beci, 1, 2)
    bectest <- mapply(testfun, xx, bec[-1, ])
    dim(bectest) <- dim(xx)
    bectest.full <- rbind(1, bectest)
    bectest.any <- apply(bectest.full, 1, function(k) any(k < 2))
    chgbout <- which(bectest.any)
    boutno <- seq(along=chgbout)
    reps <- diff(c(chgbout, nrow(x) + 1))
    rep(boutno, reps)
}

"bouts2.mleFUN" <- function(x, p, lambda1, lambda2)
{
    log(p * lambda1 * exp(-lambda1 * x) +
        (1 - p) * lambda2 * exp(-lambda2 * x))
}

"bouts2.ll" <- function(x) # 2-process Poisson
{
    function(p, lambda1, lambda2) {
        -sum(log(p * lambda1 * exp(-lambda1 * x) +
                 (1 - p) * lambda2 * exp(-lambda2 * x)))
    }
}

"bouts2.LL" <- function(x) # 2-process Poisson; transformed model
{
    function(p, lambda1, lambda2) {
        p <- unLogit(p)
        lambda1 <- exp(lambda1)
        lambda2 <- exp(lambda2)
        -sum(log(p * lambda1 * exp(-lambda1 * x) +
                 (1 - p) * lambda2 * exp(-lambda2 * x)))
    }
}

"bouts.mle" <- function(ll.fun, start, x, ...)
{
    ## Value: An mle object with fitted parameters
    ## --------------------------------------------------------------------
    ## Arguments: loglik.fun=string naming the function to fit;
    ## start=named list with starting values (exactly as given in ll.fun2,
    ## i.e. the reparameterized versions); back.convert=a list of strings
    ## naming the function applied to convert each parameter in 'start'
    ## back to the original ll.fun1; x=numeric vector with variable to
    ## model; ...=passed to mle
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    loglik.fun <- ll.fun(x)
    fit.mle <- stats4::mle(loglik.fun, start=start, ...)
    fit.mle
}

"bouts2.mleBEC" <- function(fit)
{
    ## Value: Numeric with bout ending criterion
    ## --------------------------------------------------------------------
    ## Arguments: fit=mle object with fitted 2-process model
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    coefs <- coef(fit)
    if (length(coefs) != 3) {
        stop("fit must have 3 coefficients in a 2-process model")
    }
    p_hat <- as.vector(coefs[1])
    lambda1_hat <- as.vector(coefs[2])
    lambda2_hat <- as.vector(coefs[3])
    log((p_hat * lambda1_hat) / ((1 - p_hat) * lambda2_hat)) /
        (lambda1_hat - lambda2_hat)
}

"plotBouts2.mle" <- function(fit, x, xlab="x", ylab="Log Frequency",
                             bec.lty=2, ...)
{
    ## Value: plot
    ## --------------------------------------------------------------------
    ## Arguments: fit=mle object with fitted 2-process model, x=numeric
    ## vector with observed data; xlab=ylab=strings for titles;
    ## bec.lty=line type for bec; ...=args to curve().
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    coefs <- coef(fit)
    p_hat <- as.vector(coefs[1])
    lambda1_hat <- as.vector(coefs[2])
    lambda2_hat <- as.vector(coefs[3])
    bec <- bouts2.mleBEC(fit)
    range.x <- range(x, na.rm=TRUE)
    curve(log(p_hat * lambda1_hat * exp(-lambda1_hat * x) +
              (1 - p_hat) * lambda2_hat * exp(-lambda2_hat * x)),
          from=range.x[1], to=range.x[2], xlab=xlab, ylab=ylab,
          xaxs="i", yaxs="i", las=1, ...)
    rug(jitter(x), side=3, ticksize=0.015, quiet=TRUE)
    becy <- bouts2.mleFUN(bec, p=p_hat, lambda1=lambda1_hat,
                          lambda2=lambda2_hat)
    usr <- par("usr")
    arrows(bec, becy, bec, usr[3], code=0, lty=bec.lty)
    legend(bec, usr[3] + ((usr[4] - usr[3]) * 0.08),
           paste("bec = ", round(bec, 2), sep=""), bty="n", cex=0.8)
    p_hat <- round(p_hat, 2)
    lambda1_hat <- round(lambda1_hat, 3)
    lambda2_hat <- round(lambda2_hat, 4)
    legend("topright",
           legend=bquote(y == log(.(p_hat) %.% .(lambda1_hat) %.%
                             e^(- .(lambda1_hat) * x) +
                             .(1 - p_hat) %.% .(lambda2_hat) %.%
                             e^(- .(lambda2_hat) * x))),
           bty="n", cex=0.8, adj=c(0, 1))
}

"plotBouts2.cdf" <- function(fit, x, draw.bec=FALSE, bec.lty=2, ...)
{
    ## Value: plot
    ## --------------------------------------------------------------------
    ## Arguments: fit=mle object with fitted 2-process model, x=numeric
    ## vector with observed data, draw.bec=logical; whether to draw the bec;
    ## bec.lty=line type for the bec reference line.
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    cdf.fun <- function(x, p, lambda1, lambda2) {
        1 - p*exp(-lambda1*x) - (1 - p)*exp(-lambda2*x)
    }
    coefs <- coef(fit)
    if (!length(coefs) %in% c(3, 4))
        stop("Number of coefficients in 'fit' must be 3 or 4")
    if (length(coefs) == 4) {
        p <- coefs[1] / (coefs[1] + coefs[3])
        coefs <- c(p, coefs[2], coefs[4])
    }
    x <- log1p(x)
    x.ecdf <- ecdf(x)
    plot(x.ecdf, las=1, cex.p=0.5, pch=19, xaxt="n", ...)
    xorig.pretty <- axTicks(1, axp=c(range(exp(x)), 1), log=TRUE)
    xat <- c(0, log1p(xorig.pretty[-1]))
    axis(1, at=xat, labels=c(0, xorig.pretty[-1]))
    plot(function(x) {
        x <- expm1(x)
        cdf.fun(x, coefs[1], coefs[2], coefs[3])
    }, 0, max(x), add=TRUE)
    if (draw.bec) {
        bec <- bec2(fit)
        becy <- cdf.fun(bec, coefs[1], coefs[2], coefs[3])
        arrows(log1p(bec), becy, log1p(bec), 0, code=0, lty=bec.lty)
        legend(log1p(bec), 0.1, paste("bec = ", round(bec, 2), sep=""),
               bty="n", cex=0.8)
    }
}


## We set these here to avoid collating problems
setMethod("bec2", signature(fit="nls"), bouts2.nlsBEC)
setMethod("bec2", signature(fit="mle"), bouts2.mleBEC)
