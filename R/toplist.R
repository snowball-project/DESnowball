toplist <-
function(fs)
  ## toplist from RP feature selection algorithm
  {
    ret <- fs$selectedList
    names(ret) <- c('genes','FIT2','pvals','positive')
    print(ret)
    invisible(ret)
  }
