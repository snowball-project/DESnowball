weight.sum <- function(wm) {
    rowSums(wm, na.rm=T)
}

weight.n <- function(wm) {
    rowSums(!is.na(wm))
}

weight.mean <- function(wm) {
    rowMeans(wm, na.rm=T)
}

weight.sd <- function(wm) {
    apply(wm, 1, sd, na.rm=T)
}

