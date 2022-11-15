#install.packages("mvtnorm")
library(mvtnorm)
library(future.apply)
#--------------------------------------------------------------------------------------------------------------------------------------------
#Function for constructing IVX instrument
Instrument=function(x,delta,T){
  
  #slope coefficient
  cz=1
  slope=1-cz/(T**delta)
  
  #differences for xt
  delta_x=diff(x)
  
  #Ar-process for z with innovations being the differences of x
  z=rep(NA,T)
  z[1]=0
  for (i in 2:T){
    z[i]=slope*z[i-1]+delta_x[i-1]
  }
  
  return(z)
}

#--------------------------------------------------------------------------------------------------------------------------------------------
#Function for IVX estimator

BIVX=function(x_dm,y_dm,z_dm){
  
  #beta_IVX=sum(y_dm*z_dm)/sum(x_dm*z_dm)
  beta_IVX=(t(y_dm) %*% z_dm)/(t(x_dm) %*% z_dm)
  
  return(as.numeric(beta_IVX))
}

#--------------------------------------------------------------------------------------------------------------------------------------------
#function for IVX Waldstatistic(beta=0)

WaldIVX=function(x,y,z,T){
  
  #demean
  z_dmean=z-mean(z)
  x_dmean=x-mean(x)
  y_dmean=y-mean(y)
  
  #construct Estimator
  beta_IVX=BIVX(x_dm=x_dmean,y_dm=y_dmean,z_dm=z_dmean)
  
  #compute residual variance
  sigma_tilde=sum((y_dmean-beta_IVX*x_dmean)**2)/T
  
  #compute Waldstatistic (beta=0)
  Wald=(((beta_IVX)**2)*(t(x_dmean) %*% z_dmean)**2)/(sigma_tilde*(t(z_dmean) %*% z_dmean))
  
  return(Wald)
}

#--------------------------------------------------------------------------------------------------------------------------------------------
#Computing Waldstatistic for Hypothesis of linearity

WaldA=function(gamma,alpha,beta,delta,x1,q,y,IVX,T){
  x=x1[1:T]
  #Defining Indicator functions
  I1=as.integer(q <= gamma)
  I2=as.integer(q >  gamma)
  
  
  #Xi stacking (Iit Iitxt)
  X1=matrix(c(I1, x * I1),ncol=2,nrow=T)
  X2=matrix(c(I2, x * I2),ncol=2,nrow=T)
  X=X1+X2
  Z=matrix(c(X1,X2),ncol=4,nrow=T)
  
  #OLS estimate
  theta_hat=solve(t(Z) %*% Z) %*% t(Z) %*% y
 
  #projection Matrices for Xi
  X1p=X1 %*% solve(t(X1) %*% X1) %*% t(X1)
  X2p=X2 %*% solve(t(X2) %*% X2) %*% t(X2)
  
  #residual variance 
  sigma_hat=(t(y) %*% y - t(y) %*% X1p %*% y-t(y) %*% X2p %*% y) / T

  #Restriction matrix null of linearity
  R0=matrix(c(diag(2),-diag(2)),ncol=4,nrow=2)
  
  #Wald- Test statistic
  Waldstat=(t(theta_hat) %*% t(R0) %*% solve( R0 %*% solve( t(Z) %*% Z ) %*% t(R0) ) %*% R0 %*% theta_hat) / sigma_hat
  if(IVX){
    #construct instrument from x
    z=Instrument(x1,delta,T+1)
    
    #Get IVX Waldstatistic (W(beta=0)_ivx )
    WIVX=WaldIVX(x,y,z[1:(T)],T)
    
    Waldstat=Waldstat+WIVX
    
  }
  
  #return Waldstatistic
  return(Waldstat)
} 

#--------------------------------------------------------------------------------------------------------------------------------------------
#Check over a fine grid of gamma values and find supWald statistic

SupWA=function(alpha,beta,rho,phi,sigma,delta,IVX,c,T,gam){
  
  #draw innovations from mv-normal with covariance matrix sigma
  eps <- mvtnorm::rmvnorm(n = (T+1), mean = rep(0, 3), sigma = sigma)
  
  #simulate AR(1) process for nu with ar coefficiente rho
  nu=rep(NA,T)
  nu[1]=0                                               #initialize at zero
  for (i in 2:T){
    nu[i]=rho*nu[i-1]+eps[i,2]                        # since i starts at 2 , start with eps[i-1,2] to get first entry
  }
  #simulate AR(1) process for x with ar coefficient (1-c/T)
  #from x_1 -> x_T
  x=rep(NA,T)
  x[1]=nu[1]
  for (i in 2:T){
    x[i]=(1-c/T)*x[i-1]+nu[i]
  }
  
  #simulate AR(1) process for q with ar coefficient phi
  q=rep(NA,T)
  q[1]=0
  for (i in 2:T){
    q[i]=phi*q[i-1]+eps[i,3]
  }
  
  I1_dgp=as.integer(q <= gam)
  I2_dgp=as.integer(q > gam )
  # build y -> from y_2 -> Y_T+1
  y=rep(NA,T)
  for(i in 2:(T+1)){
    y[i]=(alpha[1]+beta[1]*x[i-1]+eps[i,1])*I1_dgp[i]+(alpha[2]+beta[2]*x[i-1]+eps[i,1])*I2_dgp[i]
  }
  y=y[2:T]
  #grid of gamma values, with the lowest 10% and highest 10% of q-values being ignored/trimmed
  gamma_grid=seq(quantile(q,0.1),quantile(q,0.9),0.01)
 
  
  #Build Wald-stats for every value of gamma_grid
  WaldAs=sapply(gamma_grid,FUN=WaldA,alpha=alpha,beta=beta,delta=delta,x1=x,y=y,q=q[1:(T-1)],IVX=IVX,T=T-1)
  
  #get maximum of Waldstatistics
  SupWald=max(WaldAs)
  
  return(SupWald)
}
#--------------------------------------------------------------------------------------------------------------------------------------------

#helper function for better readability
rep_DGP=function(c,T,alpha,beta,gam,delta,rho){
  plan(multisession,workers=8)
  return(future_replicate(N,SupWA(c=c,T=T,sigma=sigma,alpha=alpha,beta=beta,rho=rho,phi=phi,delta=delta,IVX=IVX,gam=gam)))
}

#--------------------------------------------------------------------------------------------------------------------------------------------
#Simulating depending on DGP
Simulate_DGP=function(alpha,beta,gam=0,delta=NULL,rho,c,T){
  
  cn=length(c)
  Tn=length(T)
  c_grid=numeric()
  
  #build vector for mapply
  for(i in 1:cn){
    c_grid=c(c_grid,rep(c[i],Tn))
  }
  T_grid=rep(T,cn)
  
  #get result for all c´s and all T´s as a Matrix
  result=matrix(mapply(rep_DGP,c_grid,T,MoreArgs = list(alpha=alpha,beta=beta,gam=gam,delta=delta,rho=rho)),ncol=cn*Tn,nrow=N)
  
  #construct column names for convenience
  coln=rep(NA,cn*Tn)
  for (j in 1:(cn*Tn)){
    coln[j]=paste("c= ",c_grid[j],",T= ",T_grid[j])
  }
  colnames(result)=coln
  
  return(result)
}

#--------------------------------------------------------------------------------------------------------------------------------------------
#function for calculating power
power_calc=function(DGP,c,T,k=5){
  # set pi_0 to 0.5 due to symmetric sample trimming for SupWald
  pi_0=0.5
  power_DGP=rep(NA,dim(DGP)[2])
  
  #compute and save power for given set of simulations (different values for c and T)
  for (i in 1:dim(DGP)[2]){
    Asy_pval=sapply(DGP[,i],FUN=pv_sup,k=k,l=pi_0)
    
    power_DGP[i]=length(Asy_pval[which(Asy_pval<=0.025)])/length(Asy_pval)
  }
  
  #Cleaning up and naming columns and rows, to get a pleasent and easy to work with output.
  power_DGP=matrix(power_DGP,ncol=length(T),nrow=length(c))
  
  coln=rep(NA,length(T))
  rown=rep(NA,length(c))
  for(j in 1:length(T)){
    coln[j]=paste("c= ",c[j])
    rown[j]=paste("T= ",T[j])
  }
  colnames(power_DGP)=coln
  rownames(power_DGP)=rown
  
  return(power_DGP)
}

#--------------------------------------------------------------------------------------------------------------------------------------------
#calculating size (basically the same as for power, however i wanted a function to call for "size" and also this function allows for confidence level as input )
size_calc=function(conf,DGP,c,T,k=5){
  pi_0=0.5
  size_DGP=rep(NA,dim(DGP)[2])
  
  for (i in 1:dim(DGP)[2]){
    Asy_pval=sapply(DGP[,i],FUN=pv_sup,k=k,l=pi_0)
    
    size_DGP[i]=length(Asy_pval[which(Asy_pval<=conf)])/length(Asy_pval)
  }
  
  size_DGP=matrix(size_DGP,ncol=length(T),nrow=length(c))
  
  coln=rep(NA,length(T))
  rown=rep(NA,length(c))
  for(j in 1:length(T)){
    coln[j]=paste("T= ",T[j],"conf=",conf*100,"%")
  }
  for(j in 1:length(c)){
  rown[j]=paste("c= ",c[j])
  }
  colnames(size_DGP)=coln
  rownames(size_DGP)=rown
  
  return(100*size_DGP)
}
