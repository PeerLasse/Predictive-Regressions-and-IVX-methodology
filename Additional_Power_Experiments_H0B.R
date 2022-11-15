setwd("")
source("pv_sup.R")
source("SupWald-Functions.R")
#install.packages("future.apply")
#install.packages("kableExtra")
library(knitr)
library(rmarkdown)
library(kableExtra)
library(future.apply)


#--------------------------------------------------------------------------------------------------------------------------------------------
N=5000
IVX=TRUE

#covariance structure as befor
sig_ue=-0.5
sig_uuq=0.3
sig_euq=0.4
sigma=matrix(NA,ncol=3,nrow=3)
sigma[,1]=c(1,sig_ue,sig_uuq)
sigma[,2]=c(sig_ue,1,sig_euq)
sigma[,3]=c(sig_uuq,sig_euq,1)
#-------------------------------------------------------------------------------------------------------------------------------

#Define Simulation parameter:
T_grid=c(200,400)
c_grid=c(5,10)
#boundaries for delta
delta_grid=c(0.66,0.99)
rho=0.4
phi=0.5
#-------------------------------------------------------------------------------------------------------------------------------

start.time=Sys.time()
i=1
for (delta in delta_grid){
  nam1=paste("DGP1_delta",i)
  nam2=paste("DGP2_delta",i)
  nam3=paste("DGP3_delta",i)
  

  #DGP1
  alpha=c(0.01,0.01)
  beta=c(0.05,0.05)
  


  assign(nam1,Simulate_DGP(alpha=alpha,beta=beta,c=c_grid,delta =delta ,rho=rho,T=T_grid))
#-------------------------------------------------------------------------------------------------------------------------------
  #DGP2
  alpha=c(-0.03,0.25)
  beta=c(0.05,0.05)
  


  assign(nam2,Simulate_DGP(alpha=alpha,beta=beta,c=c_grid,delta =delta ,rho=rho,T=T_grid))
  print(2/6)
#-------------------------------------------------------------------------------------------------------------------------------
  #DGP3
  alpha=c(0.01,0.25)
  beta=c(0,0)
 


  assign(nam3,Simulate_DGP(alpha=alpha,beta=beta,c=c_grid,delta =delta,rho=rho ,T=T_grid))
  i=i+1
}
print("50%")
#-------------------------------------------------------------------------------------------------------------------------------    
end.time=Sys.time()
runtime=end.time-start.time
runtime


power_DGP1_1=power_calc(`DGP1_delta 1`,c_grid,T_grid,k=7)

power_DGP1_2=power_calc(`DGP1_delta 2`,c_grid,T_grid,k=7)




power_DGP2_1=power_calc(`DGP2_delta 1`,c_grid,T_grid,k=7)

power_DGP2_2=power_calc(`DGP2_delta 2`,c_grid,T_grid,k=7)




power_DGP3_1=power_calc(`DGP3_delta 1`,c_grid,T_grid,k=7)

power_DGP3_2=power_calc(`DGP3_delta 2`,c_grid,T_grid,k=7)




#complete_table=cbind(power_DGP1,power_DGP2,power_DGP3)#matrix(c(power_DGP1,power_DGP2,power_DGP3), ncol=9, nrow=3)

complete_table1=cbind(power_DGP1_1[,1],power_DGP2_1[,1],power_DGP3_1[,1],power_DGP1_1[,2],power_DGP2_1[,2],power_DGP3_1[,2])
complete_table2=cbind(power_DGP1_2[,1],power_DGP2_2[,1],power_DGP3_2[,1],power_DGP1_2[,2],power_DGP2_2[,2],power_DGP3_2[,2])

colnames(complete_table1)=c("DGP1","DGP2","DGP3","DGP1","DGP2","DGP3") 
colnames(complete_table2)=c("DGP1","DGP2","DGP3","DGP1","DGP2","DGP3") 
comp_ges=rbind(complete_table1,complete_table2)
setwd("")
#--------------------------------------------------------------------------------------------------------------------------------------------
p_comp1<-kable(complete_table1 ,format="latex",caption = "Power properties H0A delta=0.7",digits=2)%>%
  kable_styling()%>%add_header_above(c(" "=1,"c=5" = 3,"c=10" = 3),line_sep = 3,line = FALSE)
cat(p_comp1, sep="\n", file="power_B_delta_0.66.Rmd")
render("power_B_delta_0.66.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
p_comp2<-kable(complete_table2 ,format="latex",caption = "Power properties H0A delta=0.8",digits=2)%>%
  kable_styling()%>%add_header_above(c(" "=1,"c=5" = 3,"c=10" = 3),line_sep = 3,line = FALSE)
cat(p_comp2, sep="\n", file="power_B_delta_0.99.Rmd")
render("power_B_delta_0.99.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
p_comp<-kable(comp_ges ,format="latex",caption = "Power properties SupWald Bivx",digits=2)%>%
  kable_styling()%>%add_header_above(c(" "=1,"c=5" = 3,"c=10" = 3),line_sep = 3,line = FALSE)%>%
  pack_rows(group_label = "delta=0.66",1,2)%>%
  pack_rows(group_label = "delta=0.99",3,4)
cat(p_comp, sep="\n", file="power_B_delta.Rmd")
render("power_B_delta.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
setwd("")
