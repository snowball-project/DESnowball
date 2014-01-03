#' select the top list of genes
#'
#' Report the top list based on p values.
#' @param fs an object output from function \code{select.features}
#' @return a data.frame with two columns \code{RD} and \code{pvalue}
#' @export
toplist <- function(fs)
{
   ret <- fs$selectedList[,c('rd', 'pval')]
   names(ret) <- c("RD", "pvalue")
   invisible(ret)
}
