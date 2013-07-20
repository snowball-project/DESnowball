fs.rd <-
function(x,
           method="mcd",
           df=1)
  ## calculate teh robust distance and its p values based on Chisq distribution
  {
    rd <- robust.distance.signed(x,method=method)
    pvals <- pchisq(rd,df=df,lower.tail=F)
    genes <- row.names(x$weights.matrix)
    full.list <- data.frame(genes=genes,
                            rd=rd,
                            pvals=pvals,
                            positive=x$positives)[order(pvals,decreasing=F),]
    full.list
  }
