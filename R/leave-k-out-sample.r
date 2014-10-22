fs.leave.k.out.sample <-
function(r.idx,dt,classlabel,
           k=2,n,nu.idx,
           method.agreement=c("euclidean","manhattan","Rand","cRand","NMI",
             "KP","angle","diag","Jaccard","FM","gdbr", "gdbrcpp"),
           method.dist=c("pearson","kendall","spearman","standardizedEuclid",
             "euclidean","euclidean","pearson.u","kendall.u","spearman.u"),
           leave.by=c("count.class","flat","percent.class"),leave.k=1,
           return.value=c("mean","sd"))
  ## leave k out algorithm with exactly every possible combination for classlabel
  {

#    method.agreement <- match.arg(method.agreement)
	return.value<-"mean"
#    return.value <- match.arg(return.value)
    if(is.null(r.idx)) r.idx <- seq(nrow(dt))
#    leave.by <- match.arg(leave.by)
	
#    if(identical(leave.by,"flat")) {
#      nu.idx <- matrix(sample(seq(classlabel),
#                              (length(classlabel)-leave.k)*n,
#                              replace=T),
#                       nrow=n)
#    }
#    else if(identical(leave.by,"count.class")) {
#      nu.idx <- sample.classlabel.idx(classlabel,leave.k=leave.k,leave.k.mode="count",n=n)
#    }
#    else if(identical(leave.by,"percent.class")) {
#      nu.idx <- sample.classlabel.idx(classlabel,
#                                      leave.k=leave.k,
#                                      leave.k.mode="percent",
#                                      n=n)
#    }
#    else stop("Unsupported leave.by value!")
	
	## distance
	subset.dt<-dt[r.idx,]
	if(method.dist %in% c("pearson","kendall","spearman"))
		dist.matrix <- (0.5*(1-cor(subset.dt,method=method.dist,use="complete.obs")))
	else if (method.dist %in% c("pearson.u","kendall.u","spearman.u")){
		.method.dist <- sub(".u","",method.dist)
		dist.matrix <- (1-abs(cor(subset.dt,method=.method.dist,use="complete.obs")))
	}
	else if (method.dist == "standardizedEuclid") stop("Not implemented yet!")
	else dist.matrix <- as.matrix(dist(t(subset.dt),method=method.dist))
	
	nu.idxNrow<-nrow(nu.idx)
    agreement.measure <- apply(nu.idx[sample(seq(nu.idxNrow),n,replace=T),,drop=F],
                               1,
                               fs.agreement.part,
                               r.idx=r.idx,
                               dt=dist.matrix,
                               classlabel=classlabel,
                               k=k,
                               method.agreement=method.agreement,
                               method.dist=method.dist)
	envir<-sys.frame(-3)
	if(identical(return.value,"mean")) {
		envir$weightSum[r.idx]<-envir$weightSum[r.idx]+mean(agreement.measure)
		return(mean(agreement.measure))
	}   else if(identical(return.value,"sd")) {
		envir$weightSum[r.idx]<-envir$weightSum[r.idx]+mean(agreement.measure)
		return(sd(agreement.measure))
	}   else stop("Unsupported return values")
  }
