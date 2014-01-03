# DESnowball: Bagging with Distance-based Regression for Differential Gene Expression Analyses

## About
The DESnowball package implements a statistical data mining method that compares the whole genome gene 
expression profiles with respect to the presence of a recurrent genetic disturbance event (
e.g. a recurrent driver mutation) to identify the target genes affected by the event.
 
The input data for the snowball analysis are the profiling of the whole genome gene expression profiles 
and the mutation status of a recurrent genetic event on a group of samples. The analysis has 
been tested on the TCGA primary tumor samples. The minimum sample size required per group is three. 

## Installation
From R:

	library(devtools)
	install_github("DESnowball", user="snowball-project")

## Usage
Example: snowball analysis on the demo dataset included in the package 
    library(DESnowball)
    data(snowball.demoData)
    # A test run
    Bn <- 10000
    ncore <-4
    # call Snowball
    sb <- snowball(y=sb.mutation,X=sb.expression,
    	          ncore=ncore,d=100,B=Bn,
		  sample.n=1)
    # process the gene ranking and selection
    sb.sel <- select.features(sb)
    # plot the Jn values
    plotJn(sb, sb.sel)
    # get the significant gene list
    top.genes <- toplist(sb.sel)
