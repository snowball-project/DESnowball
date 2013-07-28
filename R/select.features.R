select.features <-
    function(x,
	     method='mcd',
	     cutoff.p=0.05,
	     df=1,
#	     positive=T,
	     p.adjust.method='BH')
	## x has class 'fsconcord'.
    {
	full.list <- fs.rd(x=x,
			   method=method,
			   df=df)

	selected.list <- fs.selection(full.list,
				      cutoff.p = cutoff.p,
#				      positive=positive,
				      p.adjust.method=p.adjust.method)
	ret <- list(fullList=full.list,
		    selectedList=selected.list)
	class(ret) <- 'fsfeature'
	ret
    }
