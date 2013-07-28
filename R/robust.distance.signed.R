robust.distance.signed <-
function(x,method='mcd')
  ### calculate the robust mahalanobis distance
  ### x is the output from fs.randomWeight.part
  {
    require(MASS)
    x.w <- as.matrix(x$weights)
    robcov <- cov.rob(x.w,method=method)
    rob.dist <- mahalanobis(x.w,
			    center=robcov$center,
			    cov=robcov$cov)
    rob.dist
  }
