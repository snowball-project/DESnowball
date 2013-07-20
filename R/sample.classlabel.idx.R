sample.classlabel.idx <-
function(classlabel,leave.k,n,leave.k.mode)
  ##
  {
    alist <- sample.classlabel.idx.aux2(classlabel=classlabel,
                                        leave.k=leave.k,
                                        n=n,
                                        leave.k.mode=leave.k.mode)
    for (i in seq(alist)) {
      if(i == 1) ret <- alist[[i]]
      else ret <- cbind(ret,alist[[i]])
    }
    ret
  }
