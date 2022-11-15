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
N=10

alpha=c(0.01,0.01)
beta=c(0.1,0.1)


phi=0.5

#Defining variance-covariance matrix 
sig_ue=-0.5
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
c_grid=c(5,10)
rho_grid=c(0.7,0.8,0.9)


start.time=Sys.time()
i=1
for (rho in rho_grid){
  nam=paste("Simulation_rho",i)
  assign(nam,Simulate_DGP(alpha=alpha,beta=beta,rho=rho,c=c_grid,T=T_grid))
  i=i+1
}
end.time=Sys.time()
runtime=end.time-start.time
runtime

simlist=list(`Simulation_rho 1`,`Simulation_rho 2`,`Simulation_rho 3`)
i=1
for(sim in simlist){
  namsize=paste("size_rho",i)
  for (conf in c(0.025,0.05,0.1)){
    nam=paste("size",100*conf)
    assign(nam,size_calc(conf,sim,c_grid,T_grid,k=5))
  }
  assign(namsize,cbind(`size 2.5`,`size 5`,`size 10`))
  i=i+1
}
`size_rho 1`=cbind(`size_rho 1`[,c(1,3,5)],`size_rho 1`[,c(2,4,6)])
`size_rho 2`=cbind(`size_rho 2`[,c(1,3,5)],`size_rho 2`[,c(2,4,6)])
`size_rho 3`=cbind(`size_rho 3`[,c(1,3,5)],`size_rho 3`[,c(2,4,6)])

size_rho=rbind(`size_rho 1`,`size_rho 2`,`size_rho 3`)
coln=rep(c("2.5%","5%","10%"),2)
colnames(`size_rho 1`)=coln
colnames(`size_rho 2`)=coln
colnames(`size_rho 3`)=coln
colnames(size_rho) =coln
rownames(size_rho)=rep(c("c=5","c=10"),3)
setwd("")
#--------------------------------------------------------------------------------------------------------------------------------------------
#PDF Output tab
s_r1<-kable(`size_rho 1` ,format="latex",caption = "Rho = 0.7")%>%
kable_styling()%>%add_header_above(c(" "=1,"T=200" = 3, "T=400" = 3),line_sep = 3,line = FALSE)
cat("---
header-includes: \usepackage{caption}
output: pdf_document
---
\captionsetup[table]{labelformat=empty}
",s_r1, sep="\n", file="Size_A_rho1.Rmd")
render("Size_A_rho1.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
#PDF Output tab
s_r2<-kable(`size_rho 2` ,format="latex",caption = "Rho = 0.8")%>%
kable_styling()%>%add_header_above(c(" "=1,"T=200" = 3, "T=400" = 3),line_sep = 3,line = FALSE)
cat("---
header-includes: \usepackage{caption}
output: pdf_document
---
\captionsetup[table]{labelformat=empty}
",s_r2, sep="\n", file="Size_A_rho2.Rmd")
render("Size_A_rho2.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
s_r3<-kable(`size_rho 3` ,format="latex",caption = "Rho = 0.9")%>%
kable_styling()%>%add_header_above(c(" "=1,"T=200" = 3, "T=400" = 3),line_sep = 3,line = FALSE)
cat("---
header-includes: \usepackage{caption}
output: pdf_document
---
\captionsetup[table]{labelformat=empty}
",s_r3, sep="\n", file="Size_A_rho3.Rmd")
render("Size_A_rho3.Rmd",output_format = "pdf_document")
#--------------------------------------------------------------------------------------------------------------------------------------------
s_r<-kable(size_rho ,format="latex",caption = " Table : 1 Size Estimates $W_0^A$")%>%
  kable_styling()%>%add_header_above(c(" "=1,"T=200" = 3, "T=400" = 3),line_sep = 5,line = FALSE)%>%
  pack_rows(group_label = "$\rho$=0.7",1,2)%>%
  pack_rows(group_label = "$\rho$=0.8",3,4)%>%
  pack_rows(group_label = "$\rho$=0.9",5,6)
cat(s_r, sep="\n", file="Size_A_rho.Rmd")
render("Size_A_rho.Rmd",output_format = "pdf_document")

setwd("")
