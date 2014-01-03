\name{snowball}
\alias{snowball}
\title{Snowball main function}
\usage{
  snowball(y, X, ncore = 1, d = 300, B = 10000, B.i = 2000,
    sample.n = 100,
    leave.k.out = c("sample", "none", "combn"),
    leave.by = c("class.count", "whole", "class.percent"),
    leave.k = 1)
}
\arguments{
  \item{y}{indicator variable of mutation status}

  \item{X}{data.frame containing gene expression data. The
  columns of \code{X} should be aligned with \code{y}, so
  the length of \code{y} is the same as the number of the
  columns in \code{X}}

  \item{ncore}{number of nodes to spawn for parallel
  computataion. For non-parallel computation, let
  \code{ncore = 1} or \code{NULL}.}

  \item{d}{gene subset size \eqn{d} in \eqn{X_d^x}}

  \item{B}{bootstrap size, which is \eqn{B} in \eqn{J_n(x)}
  \deqn{J_n(x)=\frac{1}{B}\sum_{i=1}^{B}(\frac{1}{K}\sum_{j=1}^{K}\phi_n(g(X_{i,j}),\kappa))}}

  \item{B.i}{bootstrap size deployed on each node in
  parallel mode}

  \item{sample.n}{resampling size on the sample dimention,
  denoted as \eqn{K} in \eqn{J_n(x)}}
}
\value{
  A class of \code{snowball} containing
  \code{phi.matrix},\code{J} and \code{positives}.
  \code{phi.matrix} contains all the \eqn{\phi_(x)} values
  evaluated by snowball algorithm. \code{J} is the
  \eqn{J_n(x)} for every gene and positives is a variable
  indicating whether a specific \eqn{J_n(x)} is above or
  below the median of all \eqn{J_n(x)}'s.
}
\description{
  This is the main function to perform snowball analysis.
  It requires a minimum input with many default operating
  parameters set.
}
\examples{
require(snowball)
data(snowball.demoData)
# check the demo dataset
print(sb.mutation)
head(sb.expression)
## A test run
Bn <- 10000
ncore <-4
# call Snowball
sb <- snowball(y=sb.mutation,X=sb.expression,
	          ncore=ncore,d=100,B=Bn,
	          sample.n=1)
# process the gene ranking and selection
sb.sel <- select.features(sb)
# plot the Jn values
plot.J(sb, sb.sel)
# get the significant gene list
top.genes <- toplist(sb.sel)
}
\references{
  Yaomin Xu, Xingyi Guo, Jiayang Sun, Zhongming Zhao.
  Snowball: Resampling combined with distance-based
  regression to discover transcriptional consequences of
  driver mutation (submitted)
}

