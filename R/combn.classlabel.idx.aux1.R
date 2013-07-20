combn.classlabel.idx.aux1 <-
function(classlabel,leave.k,leave.k.mode=c("count","percent"))
  ## tapply with combn but with different count or percentage for each level of classlabel
  {
    classlabel <- factor(classlabel)
    leave.k.mode <- match.arg(leave.k.mode)
    if(length(leave.k)!=nlevels(classlabel) & length(leave.k) != 1)
      stop("length of leave.k needs to be the same as nlevels of classlabel or 1")
    else if (length(leave.k)==1 & identical(leave.k.mode,"count")) {
      ret <- tapply(seq(classlabel),classlabel,combn,m=leave.k)
    }
    else {
      ret <- list()
      for (i in seq(nlevels(classlabel))) {
        if(identical(leave.k.mode,"percent")) {
          if(length(leave.k) == 1)
            m <- floor(sum(classlabel==levels(classlabel)[i])*leave.k)
          else m <- floor(sum(classlabel==levels(classlabel)[i])*leave.k[i])
        }
        else m <- leave.k[i]
        ret[[i]] <- combn(which(classlabel==levels(classlabel)[i]),m)
      }
    }
    names(ret) <- levels(classlabel)
    ret
  }
