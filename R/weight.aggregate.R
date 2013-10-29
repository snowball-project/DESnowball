weight.aggregate <- function(dat,
			     d=300,
			     B=100,
			     k,
			     classlabel,
			     sample.n=100,
			     method.phi=c("correspondence","Rand","cRand","NMI"),
			     method.dist=c("pearson","kendall","spearman","standardizedEuclid",
					   "pfcluster","euclidean","pearson.u","kendall.u","spearman.u"),
			     leave.k.out=c("sample","none","combn"),
			     leave.by=c("class.count","whole","class.percent"),
			     leave.k=1)
{
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
    ret <- data.frame(sum=weight.sum(wm), n=weight.n(wm))
    row.names(ret) <- row.names(dat)
    ret
}


