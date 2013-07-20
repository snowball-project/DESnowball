plot.J <-
function(x,fs,pch.nonsig=21,pch.sig=19,col.pos="red",col.neg="blue")
  ## x is the output from fs.randomWeight.part
  ## fs is the output from feature selection
  {
    plot(seq(along=x$weights),
         x$weights,
         xlab="Gene Index",
         ylab="Performace Score J",
         main="pfcluster feature selection",
         type="n")
    sigs <- row.names(x$weights.matrix)%in%fs$selectedList$genes
    positives <-
      row.names(x$weights.matrix)%in%fs$selectedList$genes[fs$selectedList$positive]
    negatives <-
      row.names(x$weights.matrix)%in%fs$selectedList$genes[!fs$selectedList$positive]

    points(seq(along=x$weights)[!sigs],x$weights[!sigs],pch=pch.nonsig)
    points(seq(along=x$weights)[positives],x$weights[positives],pch=pch.sig,col=col.pos)
    points(seq(along=x$weights)[negatives],x$weights[negatives],pch=pch.sig,col=col.neg)
  }
