\name{plotJn}
\alias{plotJn}
\title{Plot Jn values}
\usage{
plotJn(x, fs, pch.nonsig = 21, pch.sig = 19, below.median = T,
  col.above = "red", col.below = "red")
}
\arguments{
  \item{x}{an output from \code{\link{snowball}}}

  \item{fs}{the corresponding output from
  \code{\link{select.features}}}

  \item{pch.nonsig}{\code{pch} of the symbols for
  non-significant genes. See \code{\link{par}} for more
  details}

  \item{pch.sig}{\code{pch} of the symbols for significant
  genes.}

  \item{below.median}{a \code{logical} value, set to
  \code{TRUE} if the genes blow the median are to be
  highlighted}

  \item{col.above}{set the highlight color for genes above
  the median}

  \item{col.below}{set the highlight color for genes below
  the median}
}
\description{
Plot the \eqn{J_n(x)} values output from
\code{\link{snowball}}, with significant genes highlighted.
See references for more details.
}

