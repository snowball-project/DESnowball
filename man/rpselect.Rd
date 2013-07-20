\name{rpselect}
\alias{rpselect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rpselect(dat, d = 300, B = 100, k, classlabel, sample.n = 100, method.phi = c("correspondence", "Rand", "cRand", "NMI"), method.dist = c("pearson", "kendall", "spearman", "standardizedEuclid", "pfcluster", "euclidean"), leave.k.out = c("sample", "none", "combn"), leave.by = c("class.count", "whole", "class.percent"), leave.k = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{classlabel}{
%%     ~~Describe \code{classlabel} here~~
}
  \item{sample.n}{
%%     ~~Describe \code{sample.n} here~~
}
  \item{method.phi}{
%%     ~~Describe \code{method.phi} here~~
}
  \item{method.dist}{
%%     ~~Describe \code{method.dist} here~~
}
  \item{leave.k.out}{
%%     ~~Describe \code{leave.k.out} here~~
}
  \item{leave.by}{
%%     ~~Describe \code{leave.by} here~~
}
  \item{leave.k}{
%%     ~~Describe \code{leave.k} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dat, d = 300, B = 100, k, classlabel, sample.n = 100, 
    method.phi = c("correspondence", "Rand", "cRand", "NMI"), 
    method.dist = c("pearson", "kendall", "spearman", "standardizedEuclid", 
        "pfcluster", "euclidean"), leave.k.out = c("sample", 
        "none", "combn"), leave.by = c("class.count", "whole", 
        "class.percent"), leave.k = 1) 
{
    method <- match.arg(method.phi)
    if (method == "correspondence") 
        method <- "euclidean"
    method.dist <- match.arg(method.dist)
    leave.k.out <- match.arg(leave.k.out)
    leave.by <- match.arg(leave.by)
    dat.nrow <- dim(dat)[1]
    weights.matrix <- matrix(ncol = B, nrow = dat.nrow)
    idx <- matrix(sample(seq(dat.nrow), size = B * d, replace = T), 
        nrow = B)
    if (identical(leave.k.out, "none")) {
        agreement.measure <- apply(idx, 1, fs.agreement.part, 
            c.idx = seq(ncol(dat)), dt = dat, classlabel = classlabel, 
            k = k, method.agreement = method, method.dist = method.dist)
    }
    else if (identical(leave.k.out, "combn")) {
        agreement.measure <- apply(idx, 1, fs.leave.k.out.combn, 
            dt = dat, classlabel = classlabel, k = k, method.agreement = method, 
            method.dist = method.dist, leave.by = leave.by, leave.k = leave.k)
    }
    else if (identical(leave.k.out, "sample")) {
        agreement.measure <- apply(idx, 1, fs.leave.k.out.sample, 
            dt = dat, classlabel = classlabel, k = k, n = sample.n, 
            method.agreement = method, method.dist = method.dist, 
            leave.by = leave.by, leave.k = leave.k)
    }
    else stop("Unsupported leave.k.out,only none,combn or sample are supported")
    for (i in seq(B)) {
        weights.matrix[idx[i, ], i] <- agreement.measure[i]
    }
    weights <- rowMeans(weights.matrix, na.rm = T)
    positives <- (weights > median(weights))
    ret <- return(list(weights.matrix = data.frame(weights.matrix, 
        row.names = row.names(dat)), weights = weights, positives = positives))
    class(ret) <- "rpselect"
    ret
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
