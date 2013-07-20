rpselect <-
function(dat,
           d=300,
           B=100,
           k,
           classlabel,
           sample.n=100,
           method.phi=c("correspondence","Rand","cRand","NMI"),
           method.dist=c("pearson","kendall","spearman","standardizedEuclid",
             "pfcluster","euclidean"),
           leave.k.out=c("sample","none","combn"),
           leave.by=c("class.count","whole","class.percent"),
           leave.k=1)
### using fs.agreement.part to weight the features based on its partition
 ## agreement with the classlabel
  {
    method <- match.arg(method.phi)
    if(method =='correspondence') method <- 'euclidean'
    method.dist <- match.arg(method.dist)
    leave.k.out <- match.arg(leave.k.out)
    leave.by <- match.arg(leave.by)
    dat.nrow <- dim(dat)[1]
    weights.matrix <- matrix(ncol=B,nrow=dat.nrow)
    idx <- matrix(sample(seq(dat.nrow),size=B*d,replace=T),
                         nrow=B)
    if(identical(leave.k.out,"none")) {
      agreement.measure <- apply(idx,
                                 1,
                                 fs.agreement.part,
                                 c.idx=seq(ncol(dat)),
                                 dt=dat,
                                 classlabel=classlabel,
                                 k=k,
                                 method.agreement=method,
                                 method.dist=method.dist)
    }
    else if (identical(leave.k.out,"combn")) {
      agreement.measure <- apply(idx,
                                 1,
                                 fs.leave.k.out.combn,
                                 dt=dat,
                                 classlabel=classlabel,
                                 k=k,
                                 method.agreement=method,
                                 method.dist=method.dist,
                                 leave.by=leave.by,
                                 leave.k=leave.k)

    }
    else if(identical(leave.k.out,"sample")) {
      agreement.measure <- apply(idx,
                                 1,
                                 fs.leave.k.out.sample,
                                 dt=dat,
                                 classlabel=classlabel,
                                 k=k,
                                 n=sample.n,
                                 method.agreement=method,
                                 method.dist=method.dist,
                                 leave.by=leave.by,
                                 leave.k=leave.k)
    }
    else stop("Unsupported leave.k.out,only none,combn or sample are supported")      
    
    for (i in seq(B)) {
      weights.matrix[idx[i,],i] <- agreement.measure[i]
    }
      
    weights <- rowMeans(weights.matrix,na.rm=T)
    positives <- (weights>median(weights))
    ret <- return(list(weights.matrix=data.frame(weights.matrix,row.names=row.names(dat)),
                weights=weights,
                positives=positives))
    class(ret) <- 'rpselect'
    ret
  }
