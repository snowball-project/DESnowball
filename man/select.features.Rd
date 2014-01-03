\name{select.features}
\alias{select.features}
\title{Compute ranking statistics, RD, and p value for gene selection}
\usage{
select.features(x, cutoff.p = 0.05, p.adjust.method = "BH")
}
\arguments{
  \item{x}{an output from the main function
  \code{\link{snowball}}}

  \item{cutoff.p}{cutoff for top gene list. This is applied
  on the multiple testing adjusted p values}

  \item{p.adjust.method}{specifies a multiple testing
  adjustment method, see \code{\link{p.adjust}} for more
  details}
}
\value{
a list with two elements - \code{fullList} and
\code{selectedList}.\code{fullLIst} is a data.frame that
contains \code{rd}, \code{pval} and \code{positive},
corresponding respectively to the RD, p value and an
indicator variable of weather the RD value is above or
below the median value. \code{selectedList} is a data.frame
that contains the same variables as those in
\code{fullList} with only the top genes that satisfy the
significance cutoff specified by \code{cutoff.p}.
}
\description{
Gene selection based on the statistical significances
according to the Snowball approach (see references for more
details).
}

