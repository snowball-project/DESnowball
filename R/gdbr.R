gdbr <- function(y,D) {
    ##D = 1 - Sim
    A = (-1/2) * D^2
    n <- length(y)
    I = diag(1, n)
    ## association matrix
    A.cen = scale(A, scale=FALSE)
    G = A.cen - rowMeans(A.cen)

    # get Fstat ((very computer-intensive!!!)
    gdbr.stat = gdbr_fstat(y, G)
    gdbr.stat
}

gdbr_fstat <- function(casecon, G) {
    # Internal function for GDBR method
    # center y
    y.new = casecon - mean(casecon)
    I = diag(1, length(casecon))
    # get projection 'hat' matrix
    H = y.new %*% solve((t(y.new) %*% y.new)) %*% t(y.new)
    # calculate F statistic
    Fstat.num = sum(diag(H %*% G %*% H))
    Fstat.denom = sum(diag((I - H) %*% G %*% t(I - H)))
    Fstat = Fstat.num / Fstat.denom	
    Fstat
}

gdbr_perm <- function(y, D, perm.n=1000) {
    if(is(y, "character")) y <- y==unique(y)[1] ## assume y is dichotomous
    idx <- sample(seq(y),
                  length(y)*perm.n,
                  replace=T)
    y.mx <- matrix(y[idx],
                   nrow=perm.n)
    res <- apply(y.mx, 1, gdbr, D=D)
    res
}

gdbr_permtest <- function(y,D,perm.n=1000) {
    if(is(y, "character")) y <- y==unique(y)[1] ## assume y is dichotomous
    fstats <- gdbr_perm(y,D,perm.n)
    pval <- sum(fstats > gdbr(y,D))/perm.n
    pval
}


