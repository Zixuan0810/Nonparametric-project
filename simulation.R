# Make cluster
cl <- makeCluster(ncores) 
doSNOW::registerDoSNOW(cl)

# Set seed
base::set.seed(seed0)
iseed <- floor(seed0+n*1234+set*56)
base::set.seed(iseed)

# Monte Carlo simulation
test <- foreach::foreach(nn = 1:nrep, .options.RNG = iseed) %dorng% {
  data = dgp(set,n)
  treat = data[,1]
  x = as.matrix(data[,-1])       # n*k covariate matrix without intercept term
  ps = glm(treat~x,family = binomial(link = "probit"),x=T)       # fit propensity score model
  newx = ps$x                    # n*(k+1) covariate matrix with intercept term
  pscore.fitted = ps$fitted.values
  res = treat - pscore.fitted # n x 1
  #### Projection-based method
  pscore = pstest::pstest(d=treat,pscore = pscore.fitted,xpscore = ps$x,model = "probit",w = "ind",dist = "Mammen",nboot = 1000)
  pvcvm = pscore$pvcvm
  pvks = pscore$pvks
  
  #### Shaikh's test
    grid = outer(pscore.fitted,pscore.fitted,"-")
    shaikh1 = shaikh(u = res,arg = grid,n,c=0.01)
    shaikh2 = shaikh(u = res,arg = grid,n,c=0.05)
    shaikh3 = shaikh(u = res,arg = grid,n,c=0.10)
    shaikh4 = shaikh(u = res,arg = grid,n,c=0.15)
  
  # #### IPW estimator for ATE
  #   # Generate potential outcome and realized outcome
  #   p = dim(ps$x)[2]
  #   u1 = rnorm(n,0,0.1)
  #   u0 = rnorm(n,0,0.1)
  #   y1 = newx%*%rep(2,p)+u1
  #   y0 = newx%*%rep(1,p)+u0
  #   y = y1*treat+y0*(1-treat)
  #   
  #   # Calculate weights
  #   w1 = treat/pscore.fitted
  #   w0 = (1-treat)/(1-pscore.fitted)
  #   
  #   ATE = mean((w1-w0)*y)
  return(cbind(pvcvm,pvks,shaikh1,shaikh2,shaikh3,shaikh4))
}

stopCluster(cl)

# Put the Monte Carlo Results in an Array
simu <- array(unlist(test), dim = c(nrow(test[[1]]), ncol(test[[1]]), length(test)))
simu <- matrix(simu,6,nrep)
cvm <- sum(simu[1,]<=0.05)/nrep
ks <- sum(simu[2,]<=0.05)/nrep
shaikh_0.01 <- sum(simu[3,]<0.05)/nrep
shaikh_0.05 <- sum(simu[4,]<0.05)/nrep
shaikh_0.10 <- sum(simu[5,]<0.05)/nrep
shaikh_0.15 <- sum(simu[6,]<0.05)/nrep
# ATE_rmse <- sqrt(mean((simu[7,]-1)^2))
# Print output
out.p <- rep(0,8)
dim(out.p) <- c(1,8)
out.p <- data.frame(out.p)
colnames(out.p) <- c("dgp","n","CvM", "KS","c=0.01", "c=0.05","c=0.10","c=0.15")
out.p[1,1] = set
out.p[1,2] = n
out.p[1,3] = cvm
out.p[1,4] = ks
out.p[1,5] = shaikh_0.01
out.p[1,6] = shaikh_0.05
out.p[1,7] = shaikh_0.10
out.p[1,8] = shaikh_0.15
# out.p[1,9] = ATE_rmse

print(out.p)


