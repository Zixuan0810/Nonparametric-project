dgp <- function(set,n) {
  #generate covariates
  if(set<=5) {
    x1 = rnorm(n,0,1)
    x2=(rnorm(n,0,1)+x1)/sqrt(2)
  } else {
    x1 = rnorm(n,0,1)
    x2 = rnorm(n,0,1)
    x3 = rnorm(n,0,1)
    x4 = rnorm(n,0,1)
    x5 = rnorm(n,0,1)
    x6 = rnorm(n,0,1)
    x7 = rnorm(n,0,1)
    x8 = rnorm(n,0,1)
    x9 = rnorm(n,0,1)
    x10 = rnorm(n,0,1)
  }
  epsilon = rnorm(n,0,1)
  #generate treatment assignment
  if (set==1){ 
    d_star = (x1+x2)/3-epsilon
    d=ifelse(d_star>0,1,0)
    ##Put this in a datamatrix
    data=data.frame(cbind(d,x1,x2))
  }
  if (set==2){ 
    d_star = -1+(x1+x2+x1*x2)/3-epsilon
    d=ifelse(d_star>0,1,0)
    ##Put this in a datamatrix
    data=data.frame(cbind(d,x1,x2))
  }
  if (set==3){ 
    d_star = -0.2+(x1^2-x2^2)/2-epsilon
    d=ifelse(d_star>0,1,0)
    ##Put this in a datamatrix
    data=data.frame(cbind(d,x1,x2))
  }
  if (set==4){ 
    d_star = (0.1+x1/3)/exp((x1+x2)/3)-epsilon
    d=ifelse(d_star>0,1,0)
    ##Put this in a datamatrix
    data=data.frame(cbind(d,x1,x2))
  }
  if (set==5){ 
    d_star = (-0.8+(x1+x2+x1*x2)/3)/exp(0.2+(x1+x2)/3)-epsilon
    d=ifelse(d_star>0,1,0)
    ##Put this in a datamatrix
    data=data.frame(cbind(d,x1,x2))
  }
  
  if (set==6){
    d_star = -(x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)/6-epsilon
    d=ifelse(d_star>0,1,0)
    ##Put this in a datamatrix
    data=data.frame(cbind(d,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
  }
  if (set==7){
    d_star = -1-(x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)/10+x1*x2/2-epsilon
    d=ifelse(d_star>0,1,0)
    ##Put this in a datamatrix
    data=data.frame(cbind(d,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
  }
  if (set==8){
    d_star = -1 -(x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)/10 + x1*(x2+x3+x4+x5)/4-epsilon    
    d=ifelse(d_star>0,1,0)
    ##Put this in a datamatrix
    data=data.frame(cbind(d,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
  }
  if (set==9){
    d_star = -1.5-(x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)/6+(x1^2+x2^2+x3^2+x4^2+x5^2+x6^2+x7^2+x8^2+x9^2+x10^2)/10-epsilon
    d=ifelse(d_star>0,1,0)
    ##Put this in a datamatrix
    data=data.frame(cbind(d,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
  }
  if (set==10){
    d_star = (-0.1+(x1+x2+x3+x4+x5)/10)/exp(-0.2*(x1+x2+x3+x4+x5+x6+x7+x8+x9+x10))-epsilon
    d=ifelse(d_star>0,1,0)
    ##Put this in a datamatrix
    data=data.frame(cbind(d,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
  }
  return(data)
}