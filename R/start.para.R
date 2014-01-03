start.para <-
function(ncore, varlist, type=NULL) {
  if(ncore>1) {
      if(is.null(type)) cl <- makeCluster(ncore)
      else cl <- makeCluster(ncore,type=type)
  }
  else cl <- NULL
 clusterExport(cl, 
		c("snowball.initexpr",varlist), 
		envir=parent.frame())
  .dump <- clusterEvalQ(cl, eval(snowball.initexpr))
  cl
}
