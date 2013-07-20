fs.leave.k.out.combn <-
function(r.idx,dt,classlabel,
           k=2,
           method.agreement=c("euclidean","manhattan","Rand","cRand","NMI",
             "KP","angle","diag","Jaccard","FM"),
           method.dist=c("pearson","kendall","spearman","standardizedEuclid",
             "pfcluster","euclidean"),
           leave.by=c("whole","class.count","class.percent"),leave.k=1,
           return.value=c("mean","sd"))
  ## leave k out algorithm with exactly every possible combination for classlabel
  {
    require(combinat)
    method.agreement <- match.arg(method.agreement)
    method.dist <- match.arg(method.dist)
    return.value <- match.arg(return.value)
    if(is.null(r.idx)) r.idx <- seq(nrow(dt))
    leave.by <- match.arg(leave.by)
    if(identical(leave.by,"whole")) {
      nu.idx <- t(combn(seq(classlabel),length(classlabel)-leave.k))
    }
    else if(identical(leave.by,"class.count")) {
      nu.idx <- combn.classlabel.idx(classlabel,leave.k=leave.k,leave.k.mode="count")
    }
    else if(identical(leave.by,"class.percent")) {
      nu.idx <- combn.classlabel.idx(classlabel,leave.k=leave.k,leave.k.mode="percent")
    }
    else stop("Unsupported leave.by value!")
    agreement.measure <- apply(nu.idx,
                               1,
                               fs.agreement.part,
                               r.idx=r.idx,
                               dt=dt,
                               classlabel=classlabel,
                               k=k,
                               method.agreement=method.agreement,
                               method.dist=method.dist)
    if(identical(return.value,"mean")) return(mean(agreement.measure))
    else if(identical(return.value,"sd")) return(sd(agreement.measure))
    else stop("Unsupported return values")
  }
