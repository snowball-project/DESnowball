\name{plot.FIT}
\alias{plot.FIT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.FIT(fs, plot.xlab = "Gene Index", plot.ylab = "FIT", plot.main = "Main Title", qqplot.ylab = "FIT^2", qqplot.xlab = "Chisq Quantitle", pch.nonsig = 21, pch.sig = 19, col.pos = "red", col.neg = "blue")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fs}{
%%     ~~Describe \code{fs} here~~
}
  \item{plot.xlab}{
%%     ~~Describe \code{plot.xlab} here~~
}
  \item{plot.ylab}{
%%     ~~Describe \code{plot.ylab} here~~
}
  \item{plot.main}{
%%     ~~Describe \code{plot.main} here~~
}
  \item{qqplot.ylab}{
%%     ~~Describe \code{qqplot.ylab} here~~
}
  \item{qqplot.xlab}{
%%     ~~Describe \code{qqplot.xlab} here~~
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
function (fs, plot.xlab = "Gene Index", plot.ylab = "FIT", plot.main = "Main Title", 
    qqplot.ylab = "FIT^2", qqplot.xlab = "Chisq Quantitle", pch.nonsig = 21, 
    pch.sig = 19, col.pos = "red", col.neg = "blue") 
{
    sx <- sort(qchisq(ppoints(fs$fullList$rd), df = 1))
    sy <- sort(fs$fullList$rd)
    sy.sign <- fs$fullList$positive[order(fs$fullList$rd)]
    sy.significances <- fs$fullList$genes[order(fs$fullList$rd)] \%in\% 
        fs$selectedList$genes
    plot(sx, sy, xlab = qqplot.xlab, ylab = qqplot.ylab, type = "n")
    points(sx[!sy.significances], sy[!sy.significances], pch = pch.nonsig)
    points(sx[sy.significances & sy.sign], sy[sy.significances & 
        sy.sign], pch = pch.sig, col = col.pos)
    points(sx[sy.significances & !sy.sign], sy[sy.significances & 
        !sy.sign], pch = pch.sig, col = col.neg)
    abline(0, 1, col = "gray25", lty = 2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
