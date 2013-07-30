plot.J <-
function(x,fs,
	 pch.nonsig=21,pch.sig=19,
	 plot.neg=F,
	 col.pos="red",col.neg="blue",
	 abs=T)
  ## x is the output from fs.randomWeight.part
  ## fs is the output from feature selection
{
    plot(seq(along=x$weights),
         if(abs) abs(x$weights) else x$weights,
         xlab="Gene Index",
         ylab="J score",
         main="Feature selection",
         type="n")
    sigs <- row.names(x)%in%row.names(fs$selectedList)
    positives <-
      row.names(x) %in% row.names(subset(fs$selectedList,subset=positive))
    points(seq(along=x$weights)[!sigs],x$weights[!sigs],pch=pch.nonsig)
    points(seq(along=x$weights)[positives],x$weights[positives],pch=pch.sig,col=col.pos)
    negatives <-	
	row.names(x) %in% row.names(subset(fs$selectedList,subset=!positive))

    if(plot.neg) {
	points(seq(along=x$weights)[negatives],x$weights[negatives],pch=pch.sig,col=col.neg)
    } else {
	points(seq(along=x$weights)[negatives],x$weights[negatives],pch=pch.nonsig)
    }
}
