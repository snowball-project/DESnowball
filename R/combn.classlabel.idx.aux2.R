combn.classlabel.idx.aux2 <-
function(x,e,...)
  ## lapply to the list x, each elemen in x is applied with one element in e,
  ##  so e and x need to have the same length
  {
    ret <- list()
    for (i in seq(x)) {
      ret[[i]] <- x[[i]][,e[i],...]
    }
    ret
  }
