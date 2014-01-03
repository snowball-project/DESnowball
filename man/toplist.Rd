\name{toplist}
\alias{toplist}
\title{select the top list of genes}
\usage{
toplist(fs)
}
\arguments{
  \item{fs}{an object output from function
  \code{select.features}}
}
\value{
a data.frame with two columns \code{RD} and \code{pvalue}
(see references for details)
}
\description{
Report the top list based on p values.
}
\references{
Yaomin Xu, Xingyi Guo, Jiayang Sun, Zhongming Zhao.
Snowball: Resampling combined with distance-based
regression to discover transcriptional consequences of
driver mutation (submitted)
}

