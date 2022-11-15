setwd("")
source("pv_sup.R")
source("SupWald-Functions.R")
#install.packages("future.apply")
#install.packages("kableExtra")
library(knitr)
library(rmarkdown)
library(kableExtra)
library(future.apply)


######################## SIMULATION WT_B  WHEN H0B IS TRUE, Exogeneity ###################################

#--------------------------------------------------------------------------------------------------------------------------------------------
N=5000
# specifying parameter
alpha=c(0.01,0.01)
#under H0_B beta1=beta2 =0 , for size tests beta1=beta2=0.1
beta=c(0,0)

#Ar-coefficients
rho=0.4
phi=0.5

############EXOGENEITY#########
#Defining variance-covariance matrix 
sig_ue=0
sig_uuq=0
sig_euq=0
sigma=matrix(NA,ncol=3,nrow=3)
sigma[,1]=c(1,sig_ue,sig_uuq)
sigma[,2]=c(sig_ue,1,sig_euq)
sigma[,3]=c(sig_uuq,sig_euq,1)

#--------------------------------------------------------------------------------------------------------------------------------------------
# ( When true , sum of IVX based statistic on beta=0 and W_A is returned )
IVX=FALSE
T_grid=200#c(200,400,800)
c_grid=1#c(1,5)

start.time=Sys.time()
Simulation=Simulate_DGP(alpha=alpha,beta=beta,rho=rho,c=c_grid,T=T_grid)
end.time=Sys.time()
runtime=end.time-start.time
runtime

#--------------------------------------------------------------------------------------------------------------------------------------------
################################### CRITICAL VALUES WT_B ###########################################
#get quantiles of simulated SUP_wald statistics
quantiles=matrix(NA,ncol=6,nrow = 6)

for (j in 1:6){
i=1
quantile=rep(NA,6)
for(q in c(0.025,0.05,0.1,0.9,0.95,0.975)){
  quantile[i]=quantile(Simulation[,j],q)
  i=i+1
}
quantiles[,j]=quantile
} 

colnames(quantiles)=c("c=1","c=1","c=1","c=5","c=5","c=5")
rownames(quantiles)=c("2.5%","5%","10%","90%","95%","97,5%")
quantiles_200=quantiles[,c(1,4)]
quantiles_400=quantiles[,c(2,5)]
quantiles_800=quantiles[,c(3,6)]

setwd("")
#--------------------------------------------------------------------------------------------------------------------------------------------
#PDF Output tab
q_200<-kable(quantiles_200, format="markdown",caption = "DGP1 T=200")
cat(q_200, sep="\n", file="Crit_B_200.Rmd")
render("Crit_B_200.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
#PDF Output tab
q_400<-kable(quantiles_400,format="markdown",caption = "DGP1 T=400")
cat(q_400, sep="\n", file="Crit_B_400.Rmd")
render("Crit_B_400.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
q_800<-kable(quantiles_800,format="markdown",caption = "DGP1 T=800")
cat(q_800, sep="\n", file="Crit_B_800.Rmd")
render("Crit_B_800.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
setwd("")

########################### Simulations using IVX Approach , endogeneity allowed #############
alpha=c(0.01,0.01)
beta=c(0,0)
#Ar-coefficients
rho=0.4
phi=0.5

############ENDOGENEITY#########
  
sig_ue=-0.8
sig_uuq=0.5
sig_euq=0.6
sigma=matrix(NA,ncol=3,nrow=3)
sigma[,1]=c(1,sig_ue,sig_uuq)
sigma[,2]=c(sig_ue,1,sig_euq)
sigma[,3]=c(sig_uuq,sig_euq,1)

#--------------------------------------------------------------------------------------------------------------------------------------------
IVX=TRUE
T_grid=c(200,400)
c_grid=c(5,10)
delta_grid=c(0.7,0.8,0.9)

start.time=Sys.time()
simlist=list(rep(NA,length(delta_grid)))
i=1
for (delta in delta_grid){
  nam=paste("Simulation_delta",i)
  assign(nam,Simulate_DGP(alpha=alpha,beta = beta,delta=delta,c=c_grid,rho=rho,T=T_grid))
  simlist[i]=nam
  i=i+1
}
end.time=Sys.time()
runtime=end.time-start.time
runtime

###########################SIZE Properties for W_TA ##################################

#calculate size for different confidence levels 
#now with 7 degrees of freedom, since delta is allowed to vary
for (conf in c(0.025,0.05,0.1)){
  nam=paste("size",100*conf)
  assign(nam,size_calc(conf,`Simulation_delta 1`,c_grid,T_grid,k=7))
}
size=cbind(`size 2.5`,`size 5`,`size 10`)
size_delta1=cbind(size[,c(1,3,5)],size[,c(2,4,6)])

for (conf in c(0.025,0.05,0.1)){
  nam=paste("size",100*conf)
  assign(nam,size_calc(conf,`Simulation_delta 2`,c_grid,T_grid,k=7))
}
size=cbind(`size 2.5`,`size 5`,`size 10`)
size_delta2=cbind(size[,c(1,3,5)],size[,c(2,4,6)])

for (conf in c(0.025,0.05,0.1)){
  nam=paste("size",100*conf)
  assign(nam,size_calc(conf,`Simulation_delta 3`,c_grid,T_grid,k=7))
}
size=cbind(`size 2.5`,`size 5`,`size 10`)
size_delta3=cbind(size[,c(1,3,5)],size[,c(2,4,6)])

Size_ges=rbind(size_delta1,size_delta2,size_delta3)
colnames(Size_ges)=c("2.5%","5%","10%","2.5%","5%","10%")
setwd("")
#--------------------------------------------------------------------------------------------------------------------------------------------
#PDF Output tab
s_ges<-kable(Size_ges ,format="latex",caption = "Size properties SupWald B,ivx for DGP 3")%>%
  kable_styling()%>% add_header_above(c(" "=1,"T=200" = 3,"T=400" = 3),line_sep = 3,line = FALSE)%>%
  pack_rows(group_label = "delta =0.7",1,2)%>% 
  pack_rows(group_label = "delta =0.8",3,4)%>% 
  pack_rows(group_label = "delta =0.9",5,6)

cat(s_ges, sep="\n", file="Size_B_DGP3.Rmd")
render("Size_B_DGP3.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------
setwd("")
