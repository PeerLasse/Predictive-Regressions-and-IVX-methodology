setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Master 4/Seminar/Simulation/Clean")
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
alpha=c(0.01,0.01)
beta=c(0,0)
#Ar-coefficients
rho=0.4
phi=0.5

############ENDOGENEITY#########
#Defining variance-covariance matrix 
sig_ue=-0.5
sig_uuq=0.3
sig_euq=0.4 
sigma=matrix(NA,ncol=3,nrow=3)
sigma[,1]=c(1,sig_ue,sig_uuq)
sigma[,2]=c(sig_ue,1,sig_euq)
sigma[,3]=c(sig_uuq,sig_euq,1)

#--------------------------------------------------------------------------------------------------------------------------------------------
IVX=TRUE
T_grid=c(200,400,1000)
c_grid=c(1,10)
delta_grid=c(0.66,0.99)

start.time=Sys.time()
i=1
for (delta in delta_grid){
  nam=paste("Simulation_delta",i)
  assign(nam,Simulate_DGP(alpha=alpha,beta=beta,delta=delta,rho=rho,c=c_grid,T=T_grid))
  i=i+1
}
end.time=Sys.time()
runtime=end.time-start.time
runtime

simlist=list(`Simulation_delta 1`,`Simulation_delta 2`)
i=1
for(sim in simlist){
  namsize=paste("size_delta",i)
  for (conf in c(0.025,0.05,0.1)){
    nam=paste("size",100*conf)
    assign(nam,size_calc(conf,sim,c_grid,T_grid,k=7))
  }
  assign(namsize,cbind(`size 2.5`,`size 5`,`size 10`))
  i=i+1
}
`size_delta 1`=cbind(`size_delta 1`[,c(1,4,7)],`size_delta 1`[,c(2,5,8)],`size_delta 1`[,c(3,6,9)])
`size_delta 2`=cbind(`size_delta 2`[,c(1,4,7)],`size_delta 2`[,c(2,5,8)],`size_delta 2`[,c(3,6,9)])


coln=rep(c("2.5%","5%","10%"),3)
colnames(`size_delta 1`)=coln
colnames(`size_delta 2`)=coln


table=rbind(`size_delta 1`,`size_delta 2`)

setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Master 4/Seminar/Simulation/Clean/outputs/additional")
#--------------------------------------------------------------------------------------------------------------------------------------------
#PDF Output tab
s_d1<-kable(`size_delta 1` ,format="latex",caption = "delta = 0.66")%>%
  kable_styling()%>%add_header_above(c(" "=1,"T=200" = 3, "T=400" = 3,"T=1000"=3),line_sep = 3,line = FALSE)
cat(s_d1, sep="\n", file="Size_B_delta1.Rmd")
render("Size_B_delta1.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
s_d2<-kable(`size_delta 2` ,format="latex",caption = "delta = 0.99")%>%
  kable_styling()%>%add_header_above(c(" "=1,"T=200" = 3, "T=400" = 3,"T=1000"=3),line_sep = 3,line = FALSE)
cat(s_d2, sep="\n", file="Size_B_delta2.Rmd")
render("Size_B_delta2.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
s_d<-kable(table ,format="latex",caption = "Size Estimates H0B")%>%
  kable_styling()%>%add_header_above(c(" "=1,"T=200" = 3, "T=400" = 3,"T=1000"=3),line_sep = 3,line = FALSE)%>%
  pack_rows(group_label = "$delta=0.66$",1,2)%>%
  pack_rows(group_label = "$delta=0.99$",3,4)

cat(s_d, sep="\n", file="Size_B_delta.Rmd")
render("Size_B_delta.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------

setwd("C:/Users/peerl/OneDrive/Desktop/Studium/Master 4/Seminar/Simulation/Clean")