setwd("")
source("pv_sup.R")
source("SupWald-Functions.R")
#install.packages("future.apply")
#install.packages("kableExtra")
library(knitr)
library(rmarkdown)
library(kableExtra)
library(future.apply)


######################## SIMULATION WT_A  WHEN H0A IS TRUE ###################################

#--------------------------------------------------------------------------------------------------------------------------------------------
N=5000
# specifying parameter
alpha=c(0.01,0.01)
#under H0_B beta1=beta2 =0 , for size tests beta1=beta2=0. 1
beta=c(0.1,0.1)

#Ar-coefficients
rho=0.4
phi=0.5

#Defining variance-covariance matrix 
sig_ue=-0.1
sig_uuq=0.3
sig_euq=0.4
sigma=matrix(NA,ncol=3,nrow=3)
sigma[,1]=c(1,sig_ue,sig_uuq)
sigma[,2]=c(sig_ue,1,sig_euq)
sigma[,3]=c(sig_uuq,sig_euq,1)

#--------------------------------------------------------------------------------------------------------------------------------------------
#Set to TRUE when simulating H0_B , set to FALSE for H0_A   ( When true , sum of IVX based statistic on beta=0 and W_A is returned )
IVX=FALSE
T_grid=c(200,400)
c_grid=c(1,5,10,20)

start.time=Sys.time()
Simulation=Simulate_DGP(alpha=alpha,beta = beta,rho=rho,c=c_grid,T=T_grid)
end.time=Sys.time()
runtime=end.time-start.time
runtime

#--------------------------------------------------------------------------------------------------------------------------------------------
################################### CRITICAL VALUES WT_A ###########################################
#get quantiles of simulated SUP_wald statistics
quantiles=matrix(NA,ncol=8,nrow = 6)

for (j in 1:8){
i=1
quantile=rep(NA,6)
for(q in c(0.025,0.05,0.1,0.9,0.95,0.975)){
  quantile[i]=quantile(Simulation[,j],q)
  i=i+1
}
quantiles[,j]=quantile
} 

colnames(quantiles)=c("c=1","c=1","c=5","c=5","c=10","c=10","c=20","c=20")
rownames(quantiles)=c("2.5%","5%","10%","90%","95%","97,5%")
quantiles_200=quantiles[,c(1,3,5,7)]
quantiles_400=quantiles[,c(2,4,6,8)]
quantiles=rbind(quantiles_200,quantiles_400)
setwd("")
#--------------------------------------------------------------------------------------------------------------------------------------------
q_all<-kable(quantiles,format="latex",caption = "Table:2 Critical values of SupWaldA using DGP5")%>%
  kable_styling()%>%
  pack_rows(group_label = "$T=200$",1,6)%>%
  pack_rows(group_label = "$T=400$",7,12)
 
cat(q_all, sep="\n", file="Crit_A_cov3.Rmd")
render("Crit_A_cov3.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
setwd("")

###########################SIZE Properties for W_TA ##################################

#calculate size for different confidence levels
for (conf in c(0.025,0.05,0.1)){
  nam=paste("size",100*conf)
  assign(nam,size_calc(conf,Simulation,c_grid,T_grid,k=6))
}
size=cbind(`size 2.5`,`size 5`,`size 10`)
size=cbind(size[,c(1,3,5)],size[,c(2,4,6)])


colnames(size)=c("2.5%","5%","10%","2.5%","5%","10%")
setwd("")
#--------------------------------------------------------------------------------------------------------------------------------------------
#PDF Output tab
s_ges<-kable(size ,format="latex",caption = "Size properties SupWald A for DGP 5")%>%
  kable_styling()%>% add_header_above(c(" "=1,"T=200" = 3,"T=400" = 3),line_sep = 3,line = FALSE)
cat(s_ges, sep="\n", file="Size_A_DGP5.Rmd")
render("Size_A_DGP5.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
setwd("")
