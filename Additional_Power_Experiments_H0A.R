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
IVX=FALSE


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
c_grid=c(1,10)
rho_grid=c(0.7,0.8)
#-------------------------------------------------------------------------------------------------------------------------------

start.time=Sys.time()
i=1
for (rho in rho_grid){
  nam1=paste("DGP1_rho",i)
  nam2=paste("DGP2_rho",i)
  nam3=paste("DGP3_rho",i)
  

  #DGP1
  alpha=c(-0.03,-0.03)
  beta=c(1.26,1.2)
  phi=0.5


  assign(nam1,Simulate_DGP(alpha=alpha,beta=beta,c=c_grid,rho=rho,T=T_grid))

#-------------------------------------------------------------------------------------------------------------------------------
  #DGP2
  alpha=c(-0.03,0.15)
  beta=c(1.26,1.2)
  phi=0.5


  assign(nam2,Simulate_DGP(alpha=alpha,beta=beta,c=c_grid,rho=rho,T=T_grid))
  print("1/3")
#-------------------------------------------------------------------------------------------------------------------------------
  #DGP3
  alpha=c(-0.03,0.25)
  beta=c(1.26,1.26)
  phi=0.5


  assign(nam3,Simulate_DGP(alpha=alpha,beta=beta,c=c_grid,rho=rho,T=T_grid))
  i=i+1
  print("50%")
}

#-------------------------------------------------------------------------------------------------------------------------------    
end.time=Sys.time()
runtime=end.time-start.time
runtime


power_DGP1_1=power_calc(`DGP1_rho 1`,c_grid,T_grid)

power_DGP1_2=power_calc(`DGP1_rho 2`,c_grid,T_grid)

power_DGP1_3=power_calc(`DGP1_rho 3`,c_grid,T_grid)


power_DGP2_1=power_calc(`DGP2_rho 1`,c_grid,T_grid)

power_DGP2_2=power_calc(`DGP2_rho 2`,c_grid,T_grid)

power_DGP2_3=power_calc(`DGP2_rho 3`,c_grid,T_grid)


power_DGP3_1=power_calc(`DGP3_rho 1`,c_grid,T_grid)

power_DGP3_2=power_calc(`DGP3_rho 2`,c_grid,T_grid)

power_DGP3_3=power_calc(`DGP3_rho 3`,c_grid,T_grid)


#complete_table=cbind(power_DGP1,power_DGP2,power_DGP3)#matrix(c(power_DGP1,power_DGP2,power_DGP3), ncol=9, nrow=3)

complete_table1=cbind(power_DGP1_1[,1],power_DGP2_1[,1],power_DGP3_1[,1],power_DGP1_1[,2],power_DGP2_1[,2],power_DGP3_1[,2])
complete_table2=cbind(power_DGP1_2[,1],power_DGP2_2[,1],power_DGP3_2[,1],power_DGP1_2[,2],power_DGP2_2[,2],power_DGP3_2[,2])
complete_table3=cbind(power_DGP1_3[,1],power_DGP2_3[,1],power_DGP3_3[,1],power_DGP1_3[,2],power_DGP2_3[,2],power_DGP3_3[,2])
colnames(complete_table1)=c("DGP1","DGP2","DGP3","DGP1","DGP2","DGP3") 
colnames(complete_table2)=c("DGP1","DGP2","DGP3","DGP1","DGP2","DGP3") 
colnames(complete_table3)=c("DGP1","DGP2","DGP3","DGP1","DGP2","DGP3") 
setwd("")
#--------------------------------------------------------------------------------------------------------------------------------------------
p_comp1<-kable(complete_table1 ,format="latex",caption = "Power properties H0A Rho=0.7",digits=2)%>%
  kable_styling()%>%add_header_above(c(" "=1,"c=5" = 3, "c=10" = 3),line_sep = 3,line = FALSE)
cat("---
header-includes: \usepackage{caption}
output: pdf_document
---
\captionsetup[table]{labelformat=empty}
",p_comp1, sep="\n", file="power_A_rho_0.7.Rmd")
render("power_A_rho_0.7.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
p_comp2<-kable(complete_table2 ,format="latex",caption = "Power properties H0A Rho=0.8",digits=2)%>%
  kable_styling()%>%add_header_above(c(" "=1,"c=5" = 3, "c=10" = 3),line_sep = 3,line = FALSE)
cat("---
header-includes: \usepackage{caption}
output: pdf_document
---
\captionsetup[table]{labelformat=empty}
",p_comp2, sep="\n", file="power_A_rho_0.8.Rmd")
render("power_A_rho_0.8.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
p_comp3<-kable(complete_table3 ,format="latex",caption = "Power properties H0A Rho=0.9",digits=2)%>%
  kable_styling()%>%add_header_above(c(" "=1,"c=5" = 3, "c=10" = 3),line_sep = 3,line = FALSE)
cat("---
header-includes: \usepackage{caption}
output: pdf_document
---
\captionsetup[table]{labelformat=empty}
",p_comp3, sep="\n", file="power_A_rho_0.9.Rmd")
render("power_A_rho_0.9.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
setwd("")
