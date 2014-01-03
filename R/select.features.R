#' Select genes based on the statistical significances
#'
#' Select genes based on the statistical significances.
#' @param x an object output from the main function \code{snowball}
#' @param cutoff.p cutoff value for multiple testing adjusted p values
#' @param p.adjust.method multiple testing adjsutment methods, see \code{\link{p.adjust}} for more details
#' @return a list with two elements - \code{fullList} is a data.frame contains \code{rd}, \code{pval} and \code{positive}, corresponding to the RD, pvalue and a indicator weathc the RD values is above or below the median RD value; \code{selectedList}, a data.frame, contains the same variables as \code{fullList} with only the genes that satify the significant cutoff given by \code{cutoff.p}.
#' @export
select.features <- function(x,
			    cutoff.p=0.05,
			    p.adjust.method='BH')
{
    full.list <- fs.rd(x=x,
		       method="mcd",
		       df=1)

    selected.list <- fs.selection(full.list,
				  cutoff.p = cutoff.p,
				  p.adjust.method=p.adjust.method)
    ret <- list(fullList=full.list,
		selectedList=selected.list)
    class(ret) <- 'fsfeature'
    ret
}
