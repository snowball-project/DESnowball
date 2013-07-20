combn.classlabel.idx <-
function(classlabel,leave.k,leave.k.mode)
  ## combn on classlabel in different ways
  {
    nu.idx.byclass <- combn.classlabel.idx.aux1(classlabel=classlabel,
                                                leave.k=leave.k,
                                                leave.k.mode=leave.k.mode)
    expand.idx <- expand.grid(lapply(nu.idx.byclass,seq.ncol))
    extract.classlabel <- function(eachRow.in.expand.idx,
                                   nu.idx.byclass,classlabel)
      seq(classlabel)[-unlist(combn.classlabel.idx.aux2(nu.idx.byclass,
                                                        eachRow.in.expand.idx))]    
    ret <- t(apply(expand.idx,1,extract.classlabel,nu.idx.byclass,classlabel))
    ret
  }
