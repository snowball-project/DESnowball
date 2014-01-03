#'  Whole genome differential gene expression analysis of a driver mutation  
#' 
#'  The snowball package implmented Snowball approach (see references),
#'  a high sensitive statistical analysis method to identify gene
#'  transcriptional signatures affected by a driver mutation. Snowball
#'  utilizes resampling combined with distance-based regression to assign a
#'  robust ranking to the gene list based on their aggregated association with
#'  the presence of mutation. It also selects the significant top list as
#'  target genes for downstream data analyses or experiments.
#'
#' @name snowball-package
#' @docType package
#' @import clue MASS cluster parallel combinat 
#' @references
#' Yaomin Xu, Xingyi Guo, Jiayang Sun, Zhongming Zhao. Snowball: resampling combined with distance-based regression to discover transcriptional consequences of driver mutation (submitted)
NULL
