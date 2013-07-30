start.para <-
function(ncore, varlist, type="MPI") {
  if(ncore>1) cl <- makeCluster(ncore,type=type)
  else cl <- NULL
  clusterExport(cl, 
		c("snowball.initexpr",varlist), 
		envir=parent.frame())
  .dump <- clusterEvalQ(cl, eval(snowball.initexpr))
  ##.dump <- sfClusterEval(eval(initExpr))
  cl
}
