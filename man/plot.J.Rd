\name{plot.J}
\alias{plot.J}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.J(x, fs, pch.nonsig = 21, pch.sig = 19, col.pos = "red", col.neg = "blue")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{fs}{
%%     ~~Describe \code{fs} here~~
}
  \item{pch.nonsig}{
%%     ~~Describe \code{pch.nonsig} here~~
}
  \item{pch.sig}{
%%     ~~Describe \code{pch.sig} here~~
}
  \item{col.pos}{
%%     ~~Describe \code{col.pos} here~~
}
  \item{col.neg}{
%%     ~~Describe \code{col.neg} here~~
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
function (x, fs, pch.nonsig = 21, pch.sig = 19, col.pos = "red", 
    col.neg = "blue") 
{
    plot(seq(along = x$weights), x$weights, xlab = "Gene Index", 
        ylab = "Performace Score J", main = "pfcluster feature selection", 
        type = "n")
    sigs <- row.names(x$weights.matrix) \%in\% fs$selectedList$genes
    positives <- row.names(x$weights.matrix) \%in\% fs$selectedList$genes[fs$selectedList$positive]
    negatives <- row.names(x$weights.matrix) \%in\% fs$selectedList$genes[!fs$selectedList$positive]
    points(seq(along = x$weights)[!sigs], x$weights[!sigs], pch = pch.nonsig)
    points(seq(along = x$weights)[positives], x$weights[positives], 
        pch = pch.sig, col = col.pos)
    points(seq(along = x$weights)[negatives], x$weights[negatives], 
        pch = pch.sig, col = col.neg)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
