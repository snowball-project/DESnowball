\docType{package}
\name{DESnowball-package}
\alias{DESnowball-package}
\title{A R package implemented Snowball approach (see references)}
\description{
Genome-wide differential gene expression analysis with
respect to the presence of a recurrent driver mutation
}
\details{
The DESnowball package implements the Snowball approach
(see references). It is a differential gene expression
analysis tool that compares the whole genome gene
expression profiles measured on tumor samples with vs.
without a recurrent driver mutation.

The input data for the snowball analysis are the profiling
of the whole genome gene expression and the mutation status
of a recurrent driver mutation on a group of patient
samples. The analysis has been tested on the primary tumor
samples and the minimum sample size required per group is
three. Snowball does not require a balanced design between
groups (see references).

The main function of the package is \code{\link{snowball}},
it requires two input data, named \code{y} and \code{X},
where \code{y} is a binary vector indicating the mutation
status of the samples, and \code{X} is the gene expression
profiles with rows corresponding to genes and columns the
samples. \code{y} can be a \code{numerical},
\code{character} or \code{logical} vector. It can also be a
factor. The typical format is a character vector with two
values indicating the the mutation status of each subject.
\code{X} is expected to be a \link{data.frame} with gene
names as its row names, and typically it is after the
initial filtering and in log scale. A reasonable choice for
the initial filtering could be based on the variation of
gene expression across all the samples in the study, e.g.,
using the coefficient of variation of each gene to select
the ones with greater values than a given cutoff.

The other functions include \code{\link{plotJn}} for
visualizing gene selection, \code{\link{select.features}}
for gene ranking and statistical significance assessment,
and \code{\link{toplist}} to report the top genes based on
the user provided cutoff.
}
\references{
Yaomin Xu, Xingyi Guo, Jiayang Sun, Zhongming Zhao.
Snowball: resampling combined with distance-based
regression to discover transcriptional consequences of
driver mutation (submitted)

Xingyi Guo, Yaomin Xu, Zhongming Zhao. Driver mutation BRAF
regulates cell proliferation and apoptosis via MITF in the
pathogenesis of melanoma (submitted)
}

