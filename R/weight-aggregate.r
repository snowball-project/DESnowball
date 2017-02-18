weight.aggregate <- function(dat,
			     d=300,
			     B=100,
			     k,
			     classlabel,
			     sample.n=100,
			     method.phi=c("correspondence","Rand","cRand","NMI"),
			     method.dist=c("pearson","kendall","spearman","standardizedEuclid",
					   "euclidean","pearson.u","kendall.u","spearman.u"),
			     leave.k.out=c("sample","none","combn"),
			     leave.by=c("count.class","flat","percent.class"),
			     leave.k=1)
{
	weightSum<-rep(0,nrow(dat))
	weightN<-rep(0,nrow(dat))
	wm <- weight.matrix(dat=dat,
			d=d,
			B=B,
			k=k,
			classlabel=classlabel,
			sample.n=sample.n,
			method.phi=method.phi,
			method.dist=method.dist,
			leave.k.out=leave.k.out,
			leave.by=leave.by,
			leave.k=leave.k)
#    ret <- data.frame(sum=weight.sum(wm), n=weight.n(wm))
	ret <- data.frame(sum=weightSum, n=weightN)
    row.names(ret) <- row.names(dat)
    ret
}

weight.matrix <-
function(dat,
	 d=300,
	 B=100,
	 k,
	 classlabel,
	 sample.n=100,
	 method.phi=c("correspondence","Rand","cRand","NMI","gdbr", "gdbrcpp"),
	 method.dist=c("pearson","kendall","spearman","standardizedEuclid",
		       "euclidean","pearson.u","kendall.u","spearman.u"),
	 leave.k.out=c("sample","none","combn"),
	 leave.by=c("count.class","flat","percent.class"),
	 leave.k=1)
 ## use fs.agreement.part to weight the features based on its partition
 ## agreement with the classlabel
  {
#    method <- match.arg(method.phi)
	method<-method.phi
    if(method =='correspondence') method <- 'euclidean'
#    method.dist <- match.arg(method.dist)
#    leave.k.out <- match.arg(leave.k.out)
#    leave.by <- match.arg(leave.by)
    dat.nrow <- dim(dat)[1]
#    weights.matrix <- matrix(ncol=B,nrow=dat.nrow)
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
	
		if(identical(leave.by,"flat")) {
			nu.idx <- matrix(sample(seq(classlabel),
							(length(classlabel)-leave.k)*sample.n*B,
							replace=T),
					nrow=sample.n*B)
		}
		else if(identical(leave.by,"count.class")) {
			nu.idx <- sample.classlabel.idx(classlabel,leave.k=leave.k,leave.k.mode="count",n=sample.n*B)
		}
		else if(identical(leave.by,"percent.class")) {
			nu.idx <- sample.classlabel.idx(classlabel,
					leave.k=leave.k,
					leave.k.mode="percent",
					n=sample.n*B)
		}
		else stop("Unsupported leave.by value!")
		
      agreement.measure <- apply(idx,
                                 1,
                                 fs.leave.k.out.sample,
                                 dt=dat,
                                 classlabel=classlabel,
                                 k=k,
                                 n=sample.n,
								 nu.idx=nu.idx,
                                 method.agreement=method,
                                 method.dist=method.dist,
                                 leave.by=leave.by,
                                 leave.k=leave.k)
    }
    else stop("Unsupported leave.k.out,only none,combn or sample are supported")

	assign("weightN",as.integer(table(c(idx,1:dat.nrow)))-1,envir=sys.frame(-1))
#	weightN<<-as.integer(table(c(idx,1:dat.nrow)))-1
	return(0)
#    for (i in seq(B)) {
#      weights.matrix[idx[i,],i] <- agreement.measure[i]
#    }
#    weights.matrix
  }


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
