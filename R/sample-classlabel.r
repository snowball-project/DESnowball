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

sample.classlabel.idx.aux1 <-
function(x,leave.size,n,...)
  ## sample into a matrix format
{
    matrix(sample(x,(length(x)-leave.size)*n,...),nrow=n)
}

sample.classlabel.idx.aux2 <-
function(classlabel,leave.k,n,leave.k.mode=c('count','percent'))
  ## n is the sample size
{
#    classlabel <- factor(classlabel)
	classlabelUnique<-unique(classlabel)
#    leave.k.mode <- match.arg(leave.k.mode)
#    if(length(leave.k)!=nlevels(classlabel) & length(leave.k) != 1)
	if(length(leave.k)!=length(classlabelUnique) & length(leave.k) != 1)
	stop("length of leave.k needs to be the same as nlevels of classlabel or 1")
    else if (length(leave.k)==1 & identical(leave.k.mode,"count")) {
	ret <- tapply(seq(classlabel),
		      classlabel,
		      sample.classlabel.idx.aux1,
		      leave.size=leave.k,
		      n=n,
		      replace=T)
    }
    else {
	ret <- list()
	for (i in seq(length(classlabelUnique))) {
	    if(identical(leave.k.mode,"percent")) {
		if(length(leave.k) == 1)
		    m <- as.integer(sum(classlabel==classlabelUnique[i])*leave.k)
		else m <- as.integer(sum(classlabel==classlabelUnique[i])*leave.k[i])
	    }
	    else m <- leave.k[i]
	    ret[[i]] <- sample.classlabel.idx.aux1(which(classlabel==classlabelUnique[i]),
						   leave.size=m,n=n,replace=T)
	}
    }
    names(ret) <- classlabelUnique
    ret
}
