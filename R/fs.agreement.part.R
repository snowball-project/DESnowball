fs.agreement.part <-
function(r.idx,
	 c.idx,
	 dt,
	 dist=NULL,
	 classlabel,
	 k=2,
	 method.agreement=c("euclidean","manhattan","Rand","cRand","NMI",
			    "KP","angle","diag","Jaccard","FM",
			    "gdbr"),
	 method.dist=c("pearson","kendall","spearman","standardizedEuclid",
		       "pfcluster","euclidean")
	 )
    ## partition subset of dt[idx] and then calculate
    ## the agreement measure with the classLabel
    ## agreement method see 'agreement' in package 'clue'
{
    require(clue)
    method <- match.arg(method.agreement)
    method.dist <- match.arg(method.dist)
    subset.dt <- dt[r.idx,c.idx]
    classlabel <- classlabel[c.idx]
    ## distance
    if(method.dist %in% c("pearson","kendall","spearman"))
	dist.profile <- as.dist(0.5*(1-cor(subset.dt,method=method.dist)))
    else if (method.dist == "standardizedEuclid") stop("Not implemented yet!")
    else if (method.dist == "pfcluster")
	dist.profile <- as.dist(profile.dist(subset.dt,diss.type=1))
    else dist.profile <- dist(t(subset.dt),method=method.dist)
    ## partition using pam
    if(method %in% c("gdbr")) {
	ret <- gdbr(as.numeric(as.factor(classlabel)), dist.profile)
    } else {
	pam.cl <- pam(dist.profile,k,diss=T,cluster.only=T)
	ret <- cl_agreement(as.cl_hard_partition(pam.cl),
			    as.cl_hard_partition(classlabel),
			    method = method.agreement)
    }
    ret
}
