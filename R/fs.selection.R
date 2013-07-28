fs.selection <-
function(full.list,
           cutoff.p=0.05,
           p.adjust.method="BH"
#           positive=T
	   )
  ## select the significant features based on the p values
  {
#    if(positive) {
#      selected.list <- subset(full.list,
#                              subset=p.adjust(pvals,
#                                method=p.adjust.method) < cutoff.p & positive)
#    } else {
#      selected.list <- subset(full.list,
#                              subset=p.adjust(pvals,
#                                method=p.adjust.method) < cutoff.p & (!positive))
#    }
#    selected.list
      selected.list <- subset(full.list,
			      subset=p.adjust(pvals,
					      method=p.adjust.method) < cutoff.p)
      selected.list
  }
