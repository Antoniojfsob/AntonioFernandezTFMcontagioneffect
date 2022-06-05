#rm(list = ls())
#setwd()

####################################### LIBRARIES #############################
library(forecast)
library(tseries)
library(RiskPortfolios)
library(fBasics)
source("hurst.R")
source("fbmSim.R")
library(ggplot2)
library(glue)
library(car)
library(reshape2)
library(gridExtra)

######################### HIPÓTESIS DE MERCADOS EFICIENTES #####################
load("rend_df.rda")
load("rend_df_wdt.rda")
load("paises05_09_dep_wdt.rda")

#¿Estacionaria?
lapply(paises05_09_dep_wdt, ndiffs) 
lapply(rend_df_wdt,ndiffs)

lapply(rend_df_wdt,adf.test) 
lapply(rend_df_wdt,pp.test) 
# HO: no son estacionarias las series, H1:estacionaria

# Correlograma:
names <- colnames(paises05_09_dep_wdt)
for (i in names) {
  ggtsdisplay(rend_df[,i],main=paste(i))
}

#LJUNG BOX TEST 
for (i in 1:10){ 
  print(lapply(rend_df_wdt,Box.test,type="Ljung-Box", lag = i))
}
#HO: no autocorrelación.

#Exponente Hurst 
par(mfrow=c(2,3))
HURST<-lapply(rend_df_wdt,rsFit,doplot = T)
HURST
par(mfrow=c(1,1))

#Test BDS
lapply(rend_df_wdt, bds.test,m=10) 

# H0: i.i.d, rechazamos que sean iid

#Dependencia lineal en rendimientos al cuadrado
for (i in names) {
  ggtsdisplay(rend_df[,i]^2,main= glue("Rendimientos al cuadrado de {paste(i)}"))
  ggtsdisplay(abs(rend_df[,i]),main= glue("Rendimientos absolutos de {paste(i)}")) 
}

for (i in 1:10){ 
  print(lapply(rend_df_wdt^2,Box.test,type="Ljung-Box", lag = i))
  print(lapply(abs(rend_df_wdt),Box.test,type="Ljung-Box", lag = i))
}

#Distribución de los datos
#Test Normalidad Smirnov:
for (i in names){
  print(ks.test(rend_df_wdt[,i],pnorm))
}
#H0: los datos proceden de una distribución normal
#H1: los datos no proceden de una distribución normal


#FAT TAILS
rend_melt <- melt(rend_df,id = "Date")
histrend_list <- list() 
df <- split(rend_melt,rend_melt$variable)
for (i in seq_along(df)){
  histrend_list[[i]] <- ggplot(df[[i]], aes(x = value)) + 
    geom_histogram(aes(y =..density..), color="black", fill="white", alpha=.2) +
    geom_density(col="red") +
    ylab("Rendimientos") +
    ggtitle(names(df)[i]) +
    theme(text = element_text(size = 15)) 
}
grid.arrange(grobs = histrend_list, ncol = 4)


#QQplot
par(mfrow=c(4,3), cex = 0.7, mar=c(2,2,2,2))
for(i in names){
  qqPlot(rend_df[,i], ylab = "Value", main = paste(i)) 
}
