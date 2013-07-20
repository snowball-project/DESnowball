\name{sample.classlabel.idx}
\alias{sample.classlabel.idx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sample.classlabel.idx(classlabel, leave.k, n, leave.k.mode)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{classlabel}{
%%     ~~Describe \code{classlabel} here~~
}
  \item{leave.k}{
%%     ~~Describe \code{leave.k} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{leave.k.mode}{
%%     ~~Describe \code{leave.k.mode} here~~
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
function (classlabel, leave.k, n, leave.k.mode) 
{
    alist <- sample.classlabel.idx.aux2(classlabel = classlabel, 
        leave.k = leave.k, n = n, leave.k.mode = leave.k.mode)
    for (i in seq(alist)) {
        if (i == 1) 
            ret <- alist[[i]]
        else ret <- cbind(ret, alist[[i]])
    }
    ret
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
