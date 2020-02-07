shaikh <- function(u,arg,n,c) {
  h = c*n^(-1/8)
  kern = dnorm(arg/h)
  diag(kern) <- 0
  uu = u%*%t(u)
  vn = sum(kern*uu)/(n*(n-1)*h)
  sn = 2*sum((kern*uu)^2)/(n*(n-1)*h)
  tn = sqrt((n-1)/n)*n*sqrt(h)*vn/sqrt(sn)
  p = 2*pnorm(-abs(tn))
  return(p)
}