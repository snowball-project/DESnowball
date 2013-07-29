fs.rd <-
function(x,
	 method="mcd",
	 df=1)
  ## calculate teh robust distance and its p values based on Chisq distribution
  {
    rd <- robust.distance.signed(x,method=method)
    pvals <- pchisq(rd,df=df,lower.tail=F)
    full.list <- data.frame(rd=rd,
                            pvals=pvals,
                            positive=x$positives)[order(pvals,decreasing=F),]
    row.names(full.list) <- row.names(x)
    full.list
  }
