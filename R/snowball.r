#' Snowball main function
#'
#' This is the main function to perform snowball analysis. It requires a minimum input with many default operating parameters set.
#'
#' @param y indicator variable of mutation status
#' @param X data.frame containing gene expression data. The columns of \code{X} should be aligned with \code{y}, so the length of \code{y} is the same as the number of the columns in \code{X}
#' @param ncore number of nodes to spawn for parallel computataion. For non-parallel computation, let \code{ncore = 1} or \code{NULL}. 
#' @param d gene subset size \eqn{d} in \eqn{X_d^x}
#' @param B bootstrap size, which is \eqn{B} in \eqn{J_n(x)}
#' \deqn{J_n(x)=\frac{1}{B}\sum_{i=1}^{B}(\frac{1}{K}\sum_{j=1}^{K}\phi_n(g(X_{i,j}),\kappa))}
#' @param B.i bootstrap size deployed on each node in parallel mode 
#' @param sample.n resampling size on the sample dimention, denoted as \eqn{K} in \eqn{J_n(x)}
#' @return A class of \code{snowball} containing \code{phi.matrix},\code{J} and \code{positives}. \code{phi.matrix} contains all the \eqn{\phi_(x)} values evaluated by snowball algorithm. \code{J} is the \eqn{J_n(x)} for every gene and positives is a variable indicating whether a specific \eqn{J_n(x)} is above or below the median of all \eqn{J_n(x)}'s. 
#' @export
#' @examples
#' require(snowball)
#' data(snowball.demoData)
#' # check the demo dataset
#' print(sb.mutation)
#' head(sb.expression)
#' ## A test run
#' Bn <- 10000
#' ncore <-4
#' # call Snowball
#' sb <- snowball(y=sb.mutation,X=sb.expression,
#'	          ncore=ncore,d=100,B=Bn,
#'	          sample.n=1)
#' # process the gene ranking and selection
#' sb.sel <- select.features(sb)
#' # plot the Jn values
#' plot.J(sb, sb.sel)
#' # get the significant gene list
#' top.genes <- toplist(sb.sel)
#' @references
#' Yaomin Xu, Xingyi Guo, Jiayang Sun, Zhongming Zhao. Snowball: Resampling combined with distance-based regression to discover transcriptional consequences of driver mutation (submitted)
snowball <- function(y,
		     X,
		     ncore=1,
		     d=300,
		     B=10000,
		     B.i=2000,
		     sample.n=100,
		     leave.k.out=c("sample","none","combn"),
		     leave.by=c("class.count","whole","class.percent"),
		     leave.k=1)
{   
    ## check inputs
    if(!is(y,"factor")) y <- as.factor(y)
    stopifnot(nlevels(y)==2)
    stopifnot(length(y)==ncol(X))
 
    ## define operating parameters
    dat <- X
    d <- d
    B <- B
    k <- 2
    classlabel <- y
    method.phi <- "gdbr"
    method.dist <- "pearson"

    ## parallel mode?
    if(is.null(ncore)) ncore <- 1

    ## parallel computation 
    if(ncore > 1) {
	## spawn the children with needed parameters exported
	snowball.initexpr <- expression({
	    require(snowball)
	})
	cl <- start.para(ncore,
			 varlist=c('dat',
				   'd',
				   'B',
				   'k',
				   'classlabel',
				   'sample.n',
				   'method.phi',
				   'method.dist',
				   'leave.k.out',
				   'leave.by',
				   'leave.k'),
			 )
        ## on each node, calculate B.i phi values
	if(B.i<=0) B.cl <- unlist(lapply(clusterSplit(cl, seq(B)),length))
	else B.cl <- c(rep(B.i, B%/%B.i), if(B%%B.i>0) B%%B.i else NULL) 
	.arg <- parLapply(cl, B.cl, function(x) weight.aggregate(dat=dat,
								 d=d,
								 B=x,
								 k=k,
								 classlabel=classlabel,
								 sample.n=sample.n,
								 method.phi=method.phi,
								 method.dist=method.dist,
								 leave.k.out=leave.k.out,
								 leave.by=leave.by,
								 leave.k=leave.k))
	stop.para(cl)
	## campuate Jn 
	for(i in seq(along=.arg)) {
	    if(i==1) {
		.sum <- .arg[[1]]$sum
		.n <- .arg[[1]]$n
	    } else {
		.sum <- .sum+.arg[[i]]$sum
		.n <- .n+.arg[[i]]$n
	}}
	weights <- .sum/.n
    } else {
	## non-parallel
	## calculate phi 
	weights.agg <- weight.aggregate(dat=dat,
					d=d,
					B=B,
					k=k,
					classlabel=classlabel,
					sample.n=sample.n,
					method.phi=method.phi,
					method.dist=method.dist,
					leave.k.out=leave.k.out,
					leave.by=leave.by,
					leave.k=leave.k)
        ## calculate Jn
	weights <- with(weights.agg, sum/n)
    }
    ## report if there is an infufficient resampling, indicated by NA values in weights
    if(sum(is.nan(weights))>0) warning("Insufficient resampling!?")
    ## assign TRUE or FALSE based on phi values is above or below the median value
    positives <- (weights>median(weights,na.rm=T))
    ## output
    ret <- data.frame(weights=weights,
		      positives=positives)
    row.names(ret) <- row.names(dat)
    ret
}
