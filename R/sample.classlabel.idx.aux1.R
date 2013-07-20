sample.classlabel.idx.aux1 <-
function(x,leave.size,n,...)
  ## sample into a matrix format
  {
    matrix(sample(x,(length(x)-leave.size)*n,...),nrow=n)
  }
