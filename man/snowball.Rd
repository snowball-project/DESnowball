\name{snowball}
\alias{snowball}
\title{main function for Snowball analysis}
\usage{
snowball(y, X, ncore = 1, d = 300, B = 10000, B.i = 2000,
  sample.n = 100, resample.method = c("sample", "none", "combn"),
  mode.resample = c("count.class", "flat", "percent.class"), k.resample = 1)
}
\arguments{
  \item{y}{a factor variable for mutation status}

  \item{X}{data.frame containing gene expression data. The
  columns of \code{X} should be aligned with \code{y} on
  samples}

  \item{ncore}{number of processors to use for parallel
  computation. Set \code{ncore = 1} or \code{NULL} for
  non-parallel computation mode}

  \item{d}{the size of gene subset for gene level
  resampling. See references on \eqn{d} in \eqn{X_d^x}}

  \item{B}{bootstrap size, which is \eqn{B} in
  \eqn{J_n(x)}, defining the total number of gene subsets
  used to estimate \eqn{J_n},
  \deqn{J_n(x)=\frac{1}{B}\sum_{i=1}^{B}(\frac{1}{K}\sum_{j=1}^{K}\phi_n(g(X_{i,j}),\kappa))}}

  \item{B.i}{bootstrap size deployed on each child job in
  parallel mode}

  \item{sample.n}{number of samples drawn from the subject
  level resampling, denoted as \eqn{K} in \eqn{J_n(x)}. It
  is ignored if \code{resample.method="none"} or
  \code{"combn"}}

  \item{resample.method}{this defines how the subject level
  resampling is performed. The possible values are
  \code{"sample"}, \code{"none"} and \code{"combn"}. Let
  \code{resample.method = "sample"} for random sampling
  with replacement, \code{"none"} for no resampling on
  subject dimension, and \code{"combn"} for all
  combinations by permuting the subjects in each group. See
  Note for more information.}

  \item{mode.resample}{this specifies how the subjects are
  counted for subject level leave-k-out random sampling,
  and whether the stratification by group is applied. The
  possible input values are \code{"count.class"},
  \code{"percent.class"} or \code{"no"}. \code{"no"}
  implies that no stratification is applied and the
  resampling is performed on all subjects pooled together
  from the both groups. \code{"count.class"} implies the
  resampling leaves out a subset of subjects based on the
  number provided, and \code{"percent.class"} implies the
  number of subjects left out was calculated based on the
  percentage of the total subjects in each group. See Note
  for more information.}

  \item{k.resample}{A numerical value specifies the number
  of subjects left out during the subject level resampling.
  It is an integer number if \code{mode.resample =
  "count.class"} and a numerical number between 0 and 1 if
  \code{mode.resample} = "percent.class". See Note for more
  information.}
}
\value{
A data.frame containing two variables: \code{weights} and
\code{positives}. \code{weights} are the \eqn{J_n(x)}
values for all genes and positives are indicators to
whether a specific \eqn{J_n(x)} is above or below the
median of all \eqn{J_n(x)}'s.
}
\description{
This is the main function to perform snowball analysis. It
requires a minimum input with many default operating
parameters set.
}
\note{
The resampling is applied on two dimensions (see
references): gene level resamping and subject level
resampling. The gene level resampling is straightforward -
each time it takes \code{d} number of genes randomly from
all the genes in \code{X}. The subject level resampling is
specified by the combination of values given in
\code{sample.n}, \code{resample.method},
\code{mode.resample} and \code{k.resample}. The flat
resampling on all subjects regardless of grouping,
specified by letting \code{resample.method="none"}, is
simply a leave-k-out random sampling, where k is given by
\code{k.resample}. In more complex cases, the subject level
resampling can be stratified based on the groups defined on
\code{y}, in which case, \code{resample.method} takes the
value of either \code{"sample"} or \code{"combn"}. When
\code{resample.method = "sample"}, it applies a leave-k-out
random sampling within each group and finally only
\code{sample.n} samples are generated from the resampling.
When \code{resample.method = "combn"}, all possible
combinations after conditioning on the restrictions given
by \code{mode.resample} and \code{k.resample} are included.
In this case, the total number of resampled samples varies
depending on the sample size of the study.
\code{mode.resample="count.class"} or
\code{"percent.class"} defines two ways to calculate the
number of subjects to be left out in the random sampling.
The value of "count.class" indicates the exact number to be
left out and "percent.class" indicates the percentage of
total subjects to be left out. In all cases,
\code{k.resample} specifies the number of subjects left out
in the leave-k-out sampling. If \code{k.resample} is only a
scalar integer number, the subjects will be sampled with
exactly \code{k.resample} subjects left out, either across
all the subjects in the case of flat sampling, or within
each group in the case of stratified resampling by group.
Instead, if \code{k.resample} a vector with two integer
numbers, the sampling will leave out the number of subjects
from the two groups based on the two numbers provided. The
order of which number is taken for which group is based on
that the first number is assigned to the first factor level
and the second number is assigned to the second factor
level of \code{factor(y)}. Check \code{factor(y)} to see
how the two numbers in \code{k.resample} would be assigned
to the two groups. A vector with two values for
\code{k.resample} produces error if \code{mode.resample =
"flat"}. This flexible way of defining the sampling scheme
allows easy specification for balanced sample size between
groups. See references for more details.
}
\examples{
require(DESnowball)
data(snowball.demoData)
# check the demo dataset
print(sb.mutation)
head(sb.expression)
## A test run
Bn <- 10000
ncore <-4
# call Snowball
\dontrun{
sb <- snowball(y=sb.mutation,X=sb.expression,
	          ncore=ncore,d=100,B=Bn,
	          sample.n=1)
# process the gene ranking and selection
sb.sel <- select.features(sb)
# plot the Jn values
plotJn(sb, sb.sel)
# get the significant gene list
top.genes <- toplist(sb.sel)
}
}
\references{
Yaomin Xu, Xingyi Guo, Jiayang Sun, Zhongming Zhao.
Snowball: Resampling combined with distance-based
regression to discover transcriptional consequences of
driver mutation (submitted)
}

