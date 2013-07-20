\name{fs.agreement.part}
\alias{fs.agreement.part}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fs.agreement.part(r.idx, c.idx, dt, dist = NULL, classlabel, k = 2, method.agreement = c("euclidean", "manhattan", "Rand", "cRand", "NMI", "KP", "angle", "diag", "Jaccard", "FM"), method.dist = c("pearson", "kendall", "spearman", "standardizedEuclid", "pfcluster", "euclidean"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r.idx}{
%%     ~~Describe \code{r.idx} here~~
}
  \item{c.idx}{
%%     ~~Describe \code{c.idx} here~~
}
  \item{dt}{
%%     ~~Describe \code{dt} here~~
}
  \item{dist}{
%%     ~~Describe \code{dist} here~~
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
function (r.idx, c.idx, dt, dist = NULL, classlabel, k = 2, method.agreement = c("euclidean", 
    "manhattan", "Rand", "cRand", "NMI", "KP", "angle", "diag", 
    "Jaccard", "FM"), method.dist = c("pearson", "kendall", "spearman", 
    "standardizedEuclid", "pfcluster", "euclidean")) 
{
    require(clue)
    method <- match.arg(method.agreement)
    method.dist <- match.arg(method.dist)
    subset.dt <- dt[r.idx, c.idx]
    classlabel <- classlabel[c.idx]
    if (method.dist \%in\% c("pearson", "kendall", "spearman")) 
        dist.profile <- as.dist((1 - cor(subset.dt, method = method.dist))/2)
    else if (method.dist == "standardizedEuclid") 
        stop("Not implemented yet!")
    else if (method.dist == "pfcluster") 
        dist.profile <- as.dist(profile.dist(subset.dt, diss.type = 1))
    else dist.profile <- dist(t(subset.dt), method = method.dist)
    pam.cl <- pam(dist.profile, k, diss = T, cluster.only = T)
    ret <- cl_agreement(as.cl_hard_partition(pam.cl), as.cl_hard_partition(classlabel), 
        method = method.agreement)
    ret
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
