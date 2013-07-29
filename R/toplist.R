toplist <-
function(fs)
    ## toplist from RP feature selection algorithm
{
    ret <- fs$selectedList
    p.l <- row.names(subset(ret, subset=positive))
    n.l <- row.names(subset(ret, subset=!positive))
    cat("postive set:\n")
    print(p.l)
    cat("negative set: \n")
    print(n.l)
    invisible(list(positive=p.l,negative=n.l))
}
