\name{fs.leave.k.out.combn}
\alias{fs.leave.k.out.combn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fs.leave.k.out.combn(r.idx, dt, classlabel, k = 2, method.agreement = c("euclidean", "manhattan", "Rand", "cRand", "NMI", "KP", "angle", "diag", "Jaccard", "FM"), method.dist = c("pearson", "kendall", "spearman", "standardizedEuclid", "pfcluster", "euclidean"), leave.by = c("whole", "class.count", "class.percent"), leave.k = 1, return.value = c("mean", "sd"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r.idx}{
%%     ~~Describe \code{r.idx} here~~
}
  \item{dt}{
%%     ~~Describe \code{dt} here~~
}
  \item{classlabel}{
%%     ~~Describe \code{classlabel} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{method.agreement}{
%%     ~~Describe \code{method.agreement} here~~
}
  \item{method.dist}{
%%     ~~Describe \code{method.dist} here~~
}
  \item{leave.by}{
%%     ~~Describe \code{leave.by} here~~
}
  \item{leave.k}{
%%     ~~Describe \code{leave.k} here~~
}
  \item{return.value}{
%%     ~~Describe \code{return.value} here~~
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
function (r.idx, dt, classlabel, k = 2, method.agreement = c("euclidean", 
    "manhattan", "Rand", "cRand", "NMI", "KP", "angle", "diag", 
    "Jaccard", "FM"), method.dist = c("pearson", "kendall", "spearman", 
    "standardizedEuclid", "pfcluster", "euclidean"), leave.by = c("whole", 
    "class.count", "class.percent"), leave.k = 1, return.value = c("mean", 
    "sd")) 
{
    require(combinat)
    method.agreement <- match.arg(method.agreement)
    method.dist <- match.arg(method.dist)
    return.value <- match.arg(return.value)
    if (is.null(r.idx)) 
        r.idx <- seq(nrow(dt))
    leave.by <- match.arg(leave.by)
    if (identical(leave.by, "whole")) {
        nu.idx <- t(combn(seq(classlabel), length(classlabel) - 
            leave.k))
    }
    else if (identical(leave.by, "class.count")) {
        nu.idx <- combn.classlabel.idx(classlabel, leave.k = leave.k, 
            leave.k.mode = "count")
    }
    else if (identical(leave.by, "class.percent")) {
        nu.idx <- combn.classlabel.idx(classlabel, leave.k = leave.k, 
            leave.k.mode = "percent")
    }
    else stop("Unsupported leave.by value!")
    agreement.measure <- apply(nu.idx, 1, fs.agreement.part, 
        r.idx = r.idx, dt = dt, classlabel = classlabel, k = k, 
        method.agreement = method.agreement, method.dist = method.dist)
    if (identical(return.value, "mean")) 
        return(mean(agreement.measure))
    else if (identical(return.value, "sd")) 
        return(sd(agreement.measure))
    else stop("Unsupported return values")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
