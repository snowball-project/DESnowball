\name{sample.classlabel.idx.aux2}
\alias{sample.classlabel.idx.aux2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sample.classlabel.idx.aux2(classlabel, leave.k, n, leave.k.mode = c("count", "percent"))
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
function (classlabel, leave.k, n, leave.k.mode = c("count", "percent")) 
{
    classlabel <- factor(classlabel)
    leave.k.mode <- match.arg(leave.k.mode)
    if (length(leave.k) != nlevels(classlabel) & length(leave.k) != 
        1) 
        stop("length of leave.k needs to be the same as nlevels of classlabel or 1")
    else if (length(leave.k) == 1 & identical(leave.k.mode, "count")) {
        ret <- tapply(seq(classlabel), classlabel, sample.classlabel.idx.aux1, 
            leave.size = leave.k, n = n, replace = T)
    }
    else {
        ret <- list()
        for (i in seq(nlevels(classlabel))) {
            if (identical(leave.k.mode, "percent")) {
                if (length(leave.k) == 1) 
                  m <- floor(sum(classlabel == levels(classlabel)[i]) * 
                    leave.k)
                else m <- floor(sum(classlabel == levels(classlabel)[i]) * 
                  leave.k[i])
            }
            else m <- leave.k[i]
            ret[[i]] <- sample.classlabel.idx.aux1(which(classlabel == 
                levels(classlabel)[i]), leave.size = m, n = n, 
                replace = T)
        }
    }
    names(ret) <- levels(classlabel)
    ret
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
