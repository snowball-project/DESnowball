rpselect <-
function(dat,
	 d=300,
	 B=100,
	 k,
	 classlabel,
	 sample.n=100,
	 method.phi=c("correspondence","Rand","cRand","NMI","gdbr"),
	 method.dist=c("pearson","kendall","spearman","standardizedEuclid",
		       "pfcluster","euclidean","pearson.u","kendall.u","spearman.u"),
	 leave.k.out=c("sample","none","combn"),
	 leave.by=c("class.count","whole","class.percent"),
	 leave.k=1)
    ### using fs.agreement.part to weight the features based on its partition
    ## agreement with the classlabel
{
    weights.agg <- weight.aggregate(dat=dat,
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

    weights <- with(weights.agg, sum/n)
    if(sum(is.nan(weights))>0) warning("Insufficient resampling!?")
    positives <- (weights>median(weights,na.rm=T))
    ret <- data.frame(weights=weights,
		      positives=positives)
    row.names(ret) <- row.names(dat)
    ret
}
