#rm(list = ls())
#setwd()

################################### LIBRERÍAS #################################
library(glue)
library(ggplot)
library(reshape2)
library(gridExtra)
library(fBasics)
library(PerformanceAnalytics)
library(rugarch)
#--------------------- SELECCIÓN DE MODELOS UNIVARIANTES -----------------------

#load data from UnivarianteModellization.R

#Seleccionado SGARCH Normal (1,1) por defecto para todos los modelos

#USA
paste(ls(pattern = "orders"),collapse = "[['USA_Index']], ")

select_usa <- rbind(allorders_apnorm[['USA_Index']], allorders_apsknorm[['USA_Index']], 
                    allorders_apskstd[['USA_Index']], allorders_apstd[['USA_Index']], 
                    allorders_enorm[['USA_Index']], allorders_esknorm[['USA_Index']], 
                    allorders_eskstd[['USA_Index']], allorders_estd[['USA_Index']], 
                    allorders_nanorm[['USA_Index']], allorders_nasknorm[['USA_Index']], 
                    allorders_naskstd[['USA_Index']], allorders_nastd[['USA_Index']], 
                    allorders_norm[['USA_Index']], allorders_sknorm[['USA_Index']],
                    allorders_skstd[['USA_Index']], allorders_std[['USA_Index']], 
                    allorders_tnorm[['USA_Index']], allorders_tsknorm[['USA_Index']], 
                    allorders_tskstd[['USA_Index']], allorders_tstd[['USA_Index']])

#ARG
paste(ls(pattern = "orders"),collapse = "[['ARG_Price']], ")

select_arg <- rbind(allorders_apnorm[['ARG_Price']], allorders_apsknorm[['ARG_Price']],
                    allorders_apskstd[['ARG_Price']], allorders_apstd[['ARG_Price']],
                    allorders_enorm[['ARG_Price']], allorders_esknorm[['ARG_Price']],
                    allorders_eskstd[['ARG_Price']], allorders_estd[['ARG_Price']],
                    allorders_nanorm[['ARG_Price']], allorders_nasknorm[['ARG_Price']],
                    allorders_naskstd[['ARG_Price']], allorders_nastd[['ARG_Price']],
                    allorders_norm[['ARG_Price']], allorders_sknorm[['ARG_Price']],
                    allorders_skstd[['ARG_Price']], allorders_std[['ARG_Price']],
                    allorders_tnorm[['ARG_Price']], allorders_tsknorm[['ARG_Price']],
                    allorders_tskstd[['ARG_Price']], allorders_tstd[['ARG_Price']])

#EGPT
paste(ls(pattern = "orders"),collapse = "[['EGPT_Price']], ")

select_egpt <- rbind(allorders_apnorm[['EGPT_Price']], allorders_apsknorm[['EGPT_Price']], 
                     allorders_apskstd[['EGPT_Price']], allorders_apstd[['EGPT_Price']],
                     allorders_enorm[['EGPT_Price']], allorders_esknorm[['EGPT_Price']],
                     allorders_eskstd[['EGPT_Price']], allorders_estd[['EGPT_Price']],
                     allorders_nanorm[['EGPT_Price']], allorders_nasknorm[['EGPT_Price']],
                     allorders_naskstd[['EGPT_Price']], allorders_nastd[['EGPT_Price']],
                     allorders_norm[['EGPT_Price']], allorders_sknorm[['EGPT_Price']],
                     allorders_skstd[['EGPT_Price']], allorders_std[['EGPT_Price']],
                     allorders_tnorm[['EGPT_Price']], allorders_tsknorm[['EGPT_Price']],
                     allorders_tskstd[['EGPT_Price']], allorders_tstd[['EGPT_Price']])

#GHA
paste(ls(pattern = "orders"),collapse = "[['GHA_Price']], ")

select_gha <- rbind(allorders_apnorm[['GHA_Price']], allorders_apsknorm[['GHA_Price']],
                    allorders_apskstd[['GHA_Price']], allorders_apstd[['GHA_Price']],
                    allorders_enorm[['GHA_Price']], allorders_esknorm[['GHA_Price']],
                    allorders_eskstd[['GHA_Price']], allorders_estd[['GHA_Price']],
                    allorders_nanorm[['GHA_Price']], allorders_nasknorm[['GHA_Price']],
                    allorders_naskstd[['GHA_Price']], allorders_nastd[['GHA_Price']],
                    allorders_norm[['GHA_Price']], allorders_sknorm[['GHA_Price']],
                    allorders_skstd[['GHA_Price']], allorders_std[['GHA_Price']],
                    allorders_tnorm[['GHA_Price']], allorders_tsknorm[['GHA_Price']],
                    allorders_tskstd[['GHA_Price']], allorders_tstd[['GHA_Price']])

#KEN
paste(ls(pattern = "orders"),collapse = "[['KEN_Price']], ")

select_ken <- rbind(allorders_apnorm[['KEN_Price']], allorders_apsknorm[['KEN_Price']],
                    allorders_apskstd[['KEN_Price']], allorders_apstd[['KEN_Price']],
                    allorders_enorm[['KEN_Price']], allorders_esknorm[['KEN_Price']],
                    allorders_eskstd[['KEN_Price']], allorders_estd[['KEN_Price']],
                    allorders_nanorm[['KEN_Price']], allorders_nasknorm[['KEN_Price']],
                    allorders_naskstd[['KEN_Price']], allorders_nastd[['KEN_Price']],
                    allorders_norm[['KEN_Price']], allorders_sknorm[['KEN_Price']],
                    allorders_skstd[['KEN_Price']], allorders_std[['KEN_Price']],
                    allorders_tnorm[['KEN_Price']], allorders_tsknorm[['KEN_Price']],
                    allorders_tskstd[['KEN_Price']], allorders_tstd[['KEN_Price']])

#MOR
paste(ls(pattern = "orders"),collapse = "[['MOR_Price']], ")

select_mor <- rbind(allorders_apnorm[['MOR_Price']], allorders_apsknorm[['MOR_Price']],
                    allorders_apskstd[['MOR_Price']], allorders_apstd[['MOR_Price']],
                    allorders_enorm[['MOR_Price']], allorders_esknorm[['MOR_Price']],
                    allorders_eskstd[['MOR_Price']], allorders_estd[['MOR_Price']],
                    allorders_nanorm[['MOR_Price']], allorders_nasknorm[['MOR_Price']],
                    allorders_naskstd[['MOR_Price']], allorders_nastd[['MOR_Price']],
                    allorders_norm[['MOR_Price']], allorders_sknorm[['MOR_Price']],
                    allorders_skstd[['MOR_Price']], allorders_std[['MOR_Price']],
                    allorders_tnorm[['MOR_Price']], allorders_tsknorm[['MOR_Price']],
                    allorders_tskstd[['MOR_Price']], allorders_tstd[['MOR_Price']])

#NIG
paste(ls(pattern = "orders"),collapse = "[['NIG_Price']], ")

select_nig <- rbind(allorders_apnorm[['NIG_Price']], allorders_apsknorm[['NIG_Price']],
                    allorders_apskstd[['NIG_Price']], allorders_apstd[['NIG_Price']],
                    allorders_enorm[['NIG_Price']], allorders_esknorm[['NIG_Price']],
                    allorders_eskstd[['NIG_Price']], allorders_estd[['NIG_Price']],
                    allorders_nanorm[['NIG_Price']], allorders_nasknorm[['NIG_Price']],
                    allorders_naskstd[['NIG_Price']], allorders_nastd[['NIG_Price']],
                    allorders_norm[['NIG_Price']], allorders_sknorm[['NIG_Price']],
                    allorders_skstd[['NIG_Price']], allorders_std[['NIG_Price']],
                    allorders_tnorm[['NIG_Price']], allorders_tsknorm[['NIG_Price']],
                    allorders_tskstd[['NIG_Price']], allorders_tstd[['NIG_Price']])

#SOUTHAFR
paste(ls(pattern = "orders"),collapse = "[['SOUTHAFR_Price']], ")

select_southafr <- rbind(allorders_apnorm[['SOUTHAFR_Price']], allorders_apsknorm[['SOUTHAFR_Price']],
                         allorders_apskstd[['SOUTHAFR_Price']], allorders_apstd[['SOUTHAFR_Price']],
                         allorders_enorm[['SOUTHAFR_Price']], allorders_esknorm[['SOUTHAFR_Price']],
                         allorders_eskstd[['SOUTHAFR_Price']], allorders_estd[['SOUTHAFR_Price']],
                         allorders_nanorm[['SOUTHAFR_Price']], allorders_nasknorm[['SOUTHAFR_Price']],
                         allorders_naskstd[['SOUTHAFR_Price']], allorders_nastd[['SOUTHAFR_Price']],
                         allorders_norm[['SOUTHAFR_Price']], allorders_sknorm[['SOUTHAFR_Price']],
                         allorders_skstd[['SOUTHAFR_Price']], allorders_std[['SOUTHAFR_Price']],
                         allorders_tnorm[['SOUTHAFR_Price']], allorders_tsknorm[['SOUTHAFR_Price']],
                         allorders_tskstd[['SOUTHAFR_Price']], allorders_tstd[['SOUTHAFR_Price']])

#TAN
paste(ls(pattern = "orders"),collapse = "[['TAN_Price']], ")

select_tan <- rbind(allorders_apnorm[['TAN_Price']], allorders_apsknorm[['TAN_Price']],
                    allorders_apskstd[['TAN_Price']], allorders_apstd[['TAN_Price']],
                    allorders_enorm[['TAN_Price']], allorders_esknorm[['TAN_Price']],
                    allorders_eskstd[['TAN_Price']], allorders_estd[['TAN_Price']],
                    allorders_nanorm[['TAN_Price']], allorders_nasknorm[['TAN_Price']],
                    allorders_naskstd[['TAN_Price']], allorders_nastd[['TAN_Price']],
                    allorders_norm[['TAN_Price']], allorders_sknorm[['TAN_Price']],
                    allorders_skstd[['TAN_Price']], allorders_std[['TAN_Price']],
                    allorders_tnorm[['TAN_Price']], allorders_tsknorm[['TAN_Price']],
                    allorders_tskstd[['TAN_Price']], allorders_tstd[['TAN_Price']])

#TUN
paste(ls(pattern = "orders"),collapse = "[['TUN_Price']], ")

select_tun <- rbind(allorders_apnorm[['TUN_Price']], allorders_apsknorm[['TUN_Price']],
                    allorders_apskstd[['TUN_Price']], allorders_apstd[['TUN_Price']],
                    allorders_enorm[['TUN_Price']], allorders_esknorm[['TUN_Price']],
                    allorders_eskstd[['TUN_Price']], allorders_estd[['TUN_Price']],
                    allorders_nanorm[['TUN_Price']], allorders_nasknorm[['TUN_Price']],
                    allorders_naskstd[['TUN_Price']], allorders_nastd[['TUN_Price']],
                    allorders_norm[['TUN_Price']], allorders_sknorm[['TUN_Price']],
                    allorders_skstd[['TUN_Price']], allorders_std[['TUN_Price']],
                    allorders_tnorm[['TUN_Price']], allorders_tsknorm[['TUN_Price']],
                    allorders_tskstd[['TUN_Price']], allorders_tstd[['TUN_Price']])

#------------------ SELECCIÓN POR DEFECTO: GARCH (1,1) -------------------------

garchmodel_norm[["USA_Index"]][["(1,1)"]]
garchmodel_norm[["ARG_Price"]][["(1,1)"]]
garchmodel_norm[["EGPT_Price"]][["(1,1)"]]
garchmodel_norm[["GHA_Price"]][["(1,1)"]]
garchmodel_norm[["KEN_Price"]][["(1,1)"]]
garchmodel_norm[["MOR_Price"]][["(1,1)"]]
garchmodel_norm[["NIG_Price"]][["(1,1)"]]
garchmodel_norm[["SOUTHAFR_Price"]][["(1,1)"]]
garchmodel_norm[["TAN_Price"]][["(1,1)"]]
garchmodel_norm[["TUN_Price"]][["(1,1)"]]

resumenselectdefectindv <- rbind(select_usa["SGARCH Norm USA_Index (1,1)",],
                                 select_arg["SGARCH Norm ARG_Price (1,1)",],
                                 select_egpt["SGARCH Norm EGPT_Price (1,1)",],
                                 select_gha["SGARCH Norm GHA_Price (1,1)",],
                                 select_ken["SGARCH Norm KEN_Price (1,1)",],
                                 select_mor["SGARCH Norm MOR_Price (1,1)",],
                                 select_nig["SGARCH Norm NIG_Price (1,1)",],
                                 select_southafr["SGARCH Norm SOUTHAFR_Price (1,1)",],
                                 select_tan["SGARCH Norm TAN_Price (1,1)",],
                                 select_tun["SGARCH Norm TUN_Price (1,1)",]) 

#------------------ SELECCIÓN VERTICAL: Fijando el orden (1,1) -----------------

## USA

selectv_usa <- select_usa[which(
  select_usa$ARCH == "1" & select_usa$GARCH == "1"),] #20 modelos
selectv_usa <- selectv_usa[order(selectv_usa$BIC),]
selectv_usa

garchmodel_std[["USA_Index"]][["(1,1)"]]
garchmodel_skstd[["USA_Index"]][["(1,1)"]]
garchmodel_nastd[["USA_Index"]][["(1,1)"]]
garchmodel_estd[["USA_Index"]][["(1,1)"]]
garchmodel_eskstd[["USA_Index"]][["(1,1)"]]

#Gráficos
par(mfrow = c(2,2), mar=c(2,2,2,2))
for (i in c(10,11,3,9)){
  plot(garchmodel_eskstd[["USA_Index"]][["(1,1)"]], which = i)
}
par(mfrow = c(1,1))

#Seleccionamos EGARCH T-Student USA_Index (1,1)

#¿Hay algún orden mejor dentro de la familia EGARCH T-Student?
allorders_estd[["USA_Index"]][order(allorders_estd[["USA_Index"]]$BIC),]
# en términos de BIC es el mejor el modelo (1,1)
garchmodel_estd[["USA_Index"]][["(2,2)"]]
# en términos de Diagnosis, es mejor el modelo (2,2)

#Gráficos
par(mfrow = c(2,2), mar=c(2,2,2,2))
for (i in c(10,11,3,9)){
  plot(garchmodel_eskstd[["USA_Index"]][["(1,1)"]], which = i)
}
par(mfrow = c(1,1))


## ARG

selectv_arg <- select_arg[which(
  select_arg$ARCH == "1" & select_arg$GARCH == "1"),] #20 modelos
selectv_arg <- selectv_arg[order(selectv_arg$BIC),]
selectv_arg

garchmodel_estd[["ARG_Price"]][["(1,1)"]]
garchmodel_eskstd[["ARG_Price"]][["(1,1)"]]
garchmodel_tstd[["ARG_Price"]][["(1,1)"]]
garchmodel_tskstd[["ARG_Price"]][["(1,1)"]]
garchmodel_std[["ARG_Price"]][["(1,1)"]]

par(mfrow = c(2,2), mar=c(2,2,2,2), cex = 1.3)
plot(garchmodel_tskstd[["ARG_Price"]][["(1,1)"]], which = 3)
plot(garchmodel_tskstd[["ARG_Price"]][["(1,1)"]], which = 9)
plot(garchmodel_std[["ARG_Price"]][["(1,1)"]], which = 3)
plot(garchmodel_std[["ARG_Price"]][["(1,1)"]], which = 9)
par(mfrow = c(1,1)) 

#Seleccionamos SGARCH T-Student ARG_Price (1,1)

#¿Hay algún modelo mejor que el seleccionado?
allorders_std[["ARG_Price"]][order(allorders_std[["ARG_Price"]]$BIC),]


##EGPT

selectv_egpt <- select_egpt[which(
  select_egpt$ARCH == "1" & select_egpt$GARCH == "1"),] #20 modelos
selectv_egpt <- selectv_egpt[order(selectv_egpt$BIC),]
selectv_egpt

garchmodel_estd[["EGPT_Price"]][["(1,1)"]]
garchmodel_std[["EGPT_Price"]][["(1,1)"]]
garchmodel_eskstd[["EGPT_Price"]][["(1,1)"]]
garchmodel_tstd[["EGPT_Price"]][["(1,1)"]]

#Escogemos EGARCH T-Student (1,1)

#¿Hay algún modelo mejor que el seleccinado dentro de la familia?
allorders_estd[["EGPT_Price"]][order(allorders_estd[["EGPT_Price"]]$BIC),]

garchmodel_estd[["EGPT_Price"]][["(4,3)"]]

##GHA

selectv_gha <- select_gha[which(
  select_gha$ARCH == "1" & select_gha$GARCH == "1"),] #20 modelos
selectv_gha <- selectv_gha[order(selectv_gha$BIC),]
selectv_gha

garchmodel_tstd[["GHA_Price"]][["(1,1)"]]
garchmodel_tskstd[["GHA_Price"]][["(1,1)"]]
garchmodel_estd[["GHA_Price"]][["(1,1)"]]

#Escogemos EGARCH T-Student (1,1)

#¿Hay algún modelo mejor que el seleccionado en el primer paso?
allorders_estd[["GHA_Price"]][order(allorders_estd[["GHA_Price"]]$BIC),]

garchmodel_estd[["GHA_Price"]][["(2,1)"]]
garchmodel_estd[["GHA_Price"]][["(4,4)"]]
garchmodel_estd[["GHA_Price"]][["(1,2)"]]
garchmodel_estd[["GHA_Price"]][["(2,2)"]]
garchmodel_estd[["GHA_Price"]][["(3,1)"]]
garchmodel_estd[["GHA_Price"]][["(1,2)"]]

par(mfrow = c(2,1), mar=c(2,2,2,2), cex = 1)
plot(garchmodel_estd[["GHA_Price"]][["(1,2)"]], which = 11, xlab = "")
plot(garchmodel_estd[["GHA_Price"]][["(2,1)"]], which = 11, xlab = "")
par(mfrow = c(1,1))

##KEN

selectv_ken <- select_ken[which(
  select_ken$ARCH == "1" & select_ken$GARCH == "1"),] #20 modelos
selectv_ken <- selectv_ken[order(selectv_ken$BIC),]
selectv_ken

garchmodel_std[["KEN_Price"]][["(1,1)"]]
garchmodel_estd[["KEN_Price"]][["(1,1)"]]

#Escogemos SGARCH T-Student (1,1)

#¿Hay algún modelo mejor que el seleccionado en el primer paso?
allorders_std[["KEN_Price"]][order(allorders_std[["KEN_Price"]]$BIC),]

garchmodel_std[["KEN_Price"]][["(1,2)"]]
garchmodel_std[["KEN_Price"]][["(2,1)"]]
garchmodel_std[["KEN_Price"]][["(2,2)"]]


##MOR

selectv_mor <- select_mor[which(
  select_mor$ARCH == "1" & select_mor$GARCH == "1"),] #20 modelos
selectv_mor <- selectv_mor[order(selectv_mor$BIC),]
selectv_mor

garchmodel_norm[["MOR_Price"]][["(1,1)"]]
garchmodel_std[["MOR_Price"]][["(1,1)"]]
garchmodel_sknorm[["MOR_Price"]][["(1,1)"]]
garchmodel_nanorm[["MOR_Price"]][["(1,1)"]]
garchmodel_enorm[["MOR_Price"]][["(1,1)"]]

#Escogemos SGARCH Skewed Normal (1,1)

#¿Hay algún modelo mejor que el seleccionado en el primer paso?
allorders_sknorm[["MOR_Price"]][order(allorders_sknorm[["MOR_Price"]]$BIC),]

garchmodel_sknorm[["MOR_Price"]][["(2,1)"]]
garchmodel_sknorm[["MOR_Price"]][["(1,2)"]]
garchmodel_sknorm[["MOR_Price"]][["(2,2)"]]


##NIG

selectv_nig <- select_nig[which(
  select_nig$ARCH == "1" & select_nig$GARCH == "1"),] #20 modelos
selectv_nig <- selectv_nig[order(selectv_nig$BIC),]
selectv_nig

garchmodel_estd[["NIG_Price"]][["(1,1)"]]
garchmodel_eskstd[["NIG_Price"]][["(1,1)"]]
garchmodel_apstd[["NIG_Price"]][["(1,1)"]]

#¿Hay algún modelo mejor que el seleccionado en el primer paso?
allorders_estd[["NIG_Price"]][order(allorders_estd[["NIG_Price"]]$BIC),]

garchmodel_estd[["NIG_Price"]][["(2,1)"]]
garchmodel_estd[["NIG_Price"]][["(1,1)"]]


##SOUTHAFR

selectv_southafr <- select_southafr[which(
  select_southafr$ARCH == "1" & select_southafr$GARCH == "1"),] #20 modelos
selectv_southafr <- selectv_southafr[order(selectv_southafr$BIC),]
selectv_southafr

garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]
garchmodel_naskstd[["SOUTHAFR_Price"]][["(1,1)"]]
garchmodel_std[["SOUTHAFR_Price"]][["(1,1)"]]

#Escogemos SGARCH Skewed T-Student SOUTHAFR_Price (1,1) 

#¿Hay algún modelo mejor que el seleccionado en el primer paso?
allorders_skstd[["SOUTHAFR_Price"]][order(allorders_skstd[["SOUTHAFR_Price"]]$BIC),]


##TAN

selectv_tan <- select_tan[which(
  select_tan$ARCH == "1" & select_tan$GARCH == "1"),] #20 modelos
selectv_tan <- selectv_tan[order(selectv_tan$BIC),]
selectv_tan

garchmodel_nastd[["TAN_Price"]][["(1,1)"]]
garchmodel_estd[["TAN_Price"]][["(1,1)"]]
garchmodel_std[["TAN_Price"]][["(1,1)"]]

# Escogemos EGARCH T-Student TAN_Price (1,1)

#¿Hay algún modelo mejor que el seleccionado en el primer paso?
allorders_estd[["TAN_Price"]][order(allorders_estd[["TAN_Price"]]$BIC),]
garchmodel_estd[["TAN_Price"]][["(1,2)"]]


##TUN

selectv_tun <- select_tun[which(
  select_tun$ARCH == "1" & select_tun$GARCH == "1"),] #20 modelos
selectv_tun <- selectv_tun[order(selectv_tun$BIC),]
selectv_tun

garchmodel_std[["TUN_Price"]][["(1,1)"]]
garchmodel_skstd[["TUN_Price"]][["(1,1)"]]
garchmodel_norm[["TUN_Price"]][["(1,1)"]]


# Escogemos SGARCH Norm TUN_Price (1,1) 

#¿Hay algún modelo mejor que el seleccionado en el primer paso?
allorders_norm[["TUN_Price"]][order(allorders_norm[["TUN_Price"]]$BIC),]

resumenselectvindv <- rbind(select_usa["EGARCH T-Student USA_Index (2,2)",],
                            select_arg["SGARCH T-Student ARG_Price (1,1)",],
                            select_egpt["EGARCH T-Student EGPT_Price (4,3)",],
                            select_gha["EGARCH T-Student GHA_Price (1,2)",],
                            select_ken["SGARCH T-Student KEN_Price (1,1)",],
                            select_mor["SGARCH Skewed Norm MOR_Price (1,1)",],
                            select_nig["EGARCH T-Student NIG_Price (1,1)",],
                            select_southafr["SGARCH Skewed T-Student SOUTHAFR_Price (1,1)",],
                            select_tan["EGARCH T-Student TAN_Price (1,2)",],
                            select_tun["SGARCH Norm TUN_Price (1,1)",]) 


#------------------ SELECCIÓN HORIZONTAL: Fijando modelo -----------------------

rendstatics <- data.frame()
rownames(rendstatics) <- colnames(rendstatics)
rendstatics <- t(data.frame(lapply(rend_zoo,mean)))
rendstatics <- cbind(rendstatics,t(data.frame(lapply(rend_zoo,median))))
rendstatics <- cbind(rendstatics,t(data.frame(lapply(rend_zoo,max))))
rendstatics <- cbind(rendstatics,t(data.frame(lapply(rend_zoo,min))))
rendstatics <- cbind(rendstatics,t(data.frame(lapply(rend_zoo,sd))))
rendstatics <- cbind(rendstatics,t(data.frame(lapply(rend_zoo,skewness))))
rendstatics <- cbind(rendstatics,t(data.frame(lapply(rend_zoo,kurtosis))))
rendstatics <- cbind(rendstatics,t(data.frame(lapply(rend_df_wdt,function(x)ks.test(x,pnorm)$statistic))))

signbiasdf <- data.frame()
counter <- 1
for(i in colnames(rend_zoo)){
  signbiasdf[counter,1] <- signbias(garchmodel_norm[[i]][["(1,1)"]])$prob[1]
  signbiasdf[counter,2] <- signbias(garchmodel_norm[[i]][["(1,1)"]])$prob[2]
  signbiasdf[counter,3] <- signbias(garchmodel_norm[[i]][["(1,1)"]])$prob[3]
  counter <- counter + 1
}
colnames(signbiasdf) <- c("Sign Bias GARCH(1,1) (Prob)", "Positive SignBias GARCH(1,1) (Prob)",
                          "Negative SignBias GARCH(1,1) (Prob)")
rownames(signbiasdf) <- colnames(rend_zoo)

rendstatics <- cbind(rendstatics,signbiasdf)

colnames(rendstatics) <- c("Media","Mediana","Máximo","Mínimo","Desviación típica",
                           "Asimetría","Curtosis","Kolmogorov-Smirnov Test",
                           "Sign Bias GARCH(1,1) (Prob)", "Positive SignBias GARCH(1,1) (Prob)",
                           "Negative SignBias GARCH(1,1) (Prob)")

#USA

#APARCH
allorders_apskstd[["USA_Index"]][order(allorders_apskstd[["USA_Index"]]$BIC),]

garchmodel_apskstd[["USA_Index"]][["(1,2)"]]
garchmodel_apskstd[["USA_Index"]][["(2,1)"]]

#TGARCH
allorders_tskstd[["USA_Index"]][order(allorders_tskstd[["USA_Index"]]$BIC),]

garchmodel_tskstd[["USA_Index"]][["(1,1)"]]
garchmodel_tskstd[["USA_Index"]][["(2,1)"]]
garchmodel_tskstd[["USA_Index"]][["(2,2)"]]

#NAGARCH
allorders_naskstd[["USA_Index"]][order(allorders_naskstd[["USA_Index"]]$BIC),]

garchmodel_naskstd[["USA_Index"]][["(1,1)"]]
garchmodel_naskstd[["USA_Index"]][["(3,2)"]]

#EGARCH
allorders_eskstd[["USA_Index"]][order(allorders_eskstd[["USA_Index"]]$BIC),]

garchmodel_eskstd[["USA_Index"]][["(1,1)"]]l
garchmodel_eskstd[["USA_Index"]][["(2,2)"]]


##ARG

#SGARCH
allorders_std[["ARG_Price"]][order(allorders_std[["ARG_Price"]]$BIC),]

garchmodel_std[["ARG_Price"]][["(1,1)"]]


##EGPT
#APARCH
allorders_apstd[["EGPT_Price"]][order(allorders_apstd[["EGPT_Price"]]$BIC),]

garchmodel_apstd[["EGPT_Price"]][["(1,1)"]]
garchmodel_apstd[["EGPT_Price"]][["(1,2)"]]
garchmodel_apstd[["EGPT_Price"]][["(2,1)"]]
garchmodel_apstd[["EGPT_Price"]][["(2,2)"]]

#TGARCH
allorders_tstd[["EGPT_Price"]][order(allorders_tstd[["EGPT_Price"]]$BIC),]

garchmodel_tstd[["EGPT_Price"]][["(1,1)"]]
garchmodel_tstd[["EGPT_Price"]][["(1,2)"]]
garchmodel_tstd[["EGPT_Price"]][["(2,2)"]]

#NAGARCH
allorders_nastd[["EGPT_Price"]][order(allorders_nastd[["EGPT_Price"]]$BIC),]

garchmodel_nastd[["EGPT_Price"]][["(1,1)"]]
garchmodel_nastd[["EGPT_Price"]][["(1,2)"]]
garchmodel_nastd[["EGPT_Price"]][["(2,2)"]]

#EGARCH
allorders_estd[["EGPT_Price"]][order(allorders_estd[["EGPT_Price"]]$BIC),]

garchmodel_estd[["EGPT_Price"]][["(4,3)"]]

##GHA


#SGARCH
allorders_skstd[["GHA_Price"]][order(allorders_skstd[["GHA_Price"]]$BIC),]

garchmodel_skstd[["GHA_Price"]][["(1,1)"]]
garchmodel_skstd[["GHA_Price"]][["(1,2)"]]
garchmodel_skstd[["GHA_Price"]][["(2,1)"]]

##KEN

#SGARCH
allorders_skstd[["KEN_Price"]][order(allorders_skstd[["KEN_Price"]]$BIC),]

garchmodel_skstd[["KEN_Price"]][["(1,1)"]]
garchmodel_skstd[["KEN_Price"]][["(1,2)"]]
garchmodel_skstd[["KEN_Price"]][["(2,1)"]]
garchmodel_skstd[["KEN_Price"]][["(2,2)"]]

##MOROCCO

#APARCH
allorders_apstd[["MOR_Price"]][order(allorders_apstd[["MOR_Price"]]$BIC),]

garchmodel_apstd[["MOR_Price"]][["(2,2)"]]
garchmodel_apstd[["MOR_Price"]][["(1,1)"]]

#TGARCH
allorders_tstd[["MOR_Price"]][order(allorders_tstd[["MOR_Price"]]$BIC),]

garchmodel_tstd[["MOR_Price"]][["(1,1)"]]
garchmodel_tstd[["MOR_Price"]][["(2,1)"]]
garchmodel_tstd[["MOR_Price"]][["(1,2)"]]
garchmodel_tstd[["MOR_Price"]][["(2,2)"]]

#NAGARCH
allorders_nastd[["MOR_Price"]][order(allorders_nastd[["MOR_Price"]]$BIC),]

garchmodel_nastd[["MOR_Price"]][["(1,1)"]]
garchmodel_nastd[["MOR_Price"]][["(1,2)"]]
garchmodel_nastd[["MOR_Price"]][["(2,1)"]]

#EGARCH
allorders_estd[["MOR_Price"]][order(allorders_estd[["MOR_Price"]]$BIC),]

garchmodel_estd[["MOR_Price"]][["(1,1)"]]
garchmodel_estd[["MOR_Price"]][["(1,3)"]]
garchmodel_estd[["MOR_Price"]][["(1,4)"]]

##NIG

#SGARCH
allorders_skstd[["NIG_Price"]][order(allorders_skstd[["NIG_Price"]]$BIC),]

garchmodel_skstd[["NIG_Price"]][["(1,3)"]]
garchmodel_skstd[["NIG_Price"]][["(2,3)"]]
garchmodel_skstd[["NIG_Price"]][["(1,1)"]]

##SOUTHAFR

#SGARCH
allorders_skstd[["SOUTHAFR_Price"]][order(allorders_skstd[["SOUTHAFR_Price"]]$BIC),]

garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]


##TAN

#APARCH
allorders_apstd[["TAN_Price"]][order(allorders_apstd[["TAN_Price"]]$BIC),]

garchmodel_apstd[["TAN_Price"]][["(1,2)"]]
garchmodel_apstd[["TAN_Price"]][["(1,1)"]]

#TGARCH
allorders_tstd[["TAN_Price"]][order(allorders_tstd[["TAN_Price"]]$BIC),]

garchmodel_tstd[["TAN_Price"]][["(1,1)"]]
garchmodel_tstd[["TAN_Price"]][["(1,2)"]]
garchmodel_tstd[["TAN_Price"]][["(2,2)"]]

#NAGARCH
allorders_nastd[["TAN_Price"]][order(allorders_nastd[["TAN_Price"]]$BIC),]

garchmodel_nastd[["TAN_Price"]][["(1,1)"]]
garchmodel_nastd[["TAN_Price"]][["(1,2)"]]

#EGARCH
allorders_estd[["TAN_Price"]][order(allorders_estd[["TAN_Price"]]$BIC),]

garchmodel_estd[["TAN_Price"]][["(1,2)"]]
garchmodel_estd[["TAN_Price"]][["(1,1)"]]


##TUN

#SGARCH
allorders_std[["TUN_Price"]][order(allorders_std[["TUN_Price"]]$BIC),]

garchmodel_std[["TUN_Price"]][["(1,1)"]]
garchmodel_std[["TUN_Price"]][["(1,2)"]]
garchmodel_std[["TUN_Price"]][["(2,1)"]]

resumenselecthindv <- rbind(select_usa["EGARCH Skewed T-Student USA_Index (2,2)",],
                            select_arg["SGARCH T-Student ARG_Price (1,1)",],
                            select_egpt["TGARCH T-Student EGPT_Price (2,2)",],
                            select_gha["SGARCH Skewed T-Student GHA_Price (1,1)",],
                            select_ken["SGARCH Skewed T-Student KEN_Price (1,1)",],
                            select_mor["EGARCH T-Student MOR_Price (1,1)",],
                            select_nig["SGARCH Skewed T-Student NIG_Price (1,1)",],
                            select_southafr["SGARCH Skewed T-Student SOUTHAFR_Price (1,1)",],
                            select_tan["EGARCH T-Student TAN_Price (1,1)",],
                            select_tun["SGARCH T-Student TUN_Price (1,1)",]) 


#----------------------------- SELECCIÓN DIAGONAL ------------------------------

#USA
select_usa_few <- head(select_usa[order(select_usa$BIC),],30)

p1 <- ggplot(select_usa_few,aes(x = AIC, y = rownames(select_usa_few))) +
  geom_point(size = ifelse(select_usa_few$AIC > min(select_usa_few$AIC),1,3),
             color = ifelse(select_usa_few$AIC > min(select_usa_few$AIC),"black","red"),
             shape = ifelse(select_usa_few$AIC > min(select_usa_few$AIC),19,8)) +
  ylab("Modelos") + theme(axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(margin= margin(1,1,3,3))) 
p2 <- ggplot(select_usa_few,aes(x = BIC, y = rownames(select_usa_few))) +
  geom_point(size = ifelse(select_usa_few$BIC > min(select_usa_few$BIC),1,3),
             color = ifelse(select_usa_few$BIC > min(select_usa_few$BIC),"black","red"),
             shape = ifelse(select_usa_few$BIC > min(select_usa_few$BIC),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p3 <- ggplot(select_usa_few,aes(x = Shibata, y = rownames(select_usa_few))) +
  geom_point(size = ifelse(select_usa_few$Shibata > min(select_usa_few$Shibata),1,3),
             color = ifelse(select_usa_few$Shibata > min(select_usa_few$Shibata),"black","red"),
             shape = ifelse(select_usa_few$Shibata > min(select_usa_few$Shibata),19,8)) +
  ylab("Modelos") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p4 <- ggplot(select_usa_few,aes(x = `Hannan-Quinn`, y = rownames(select_usa_few))) +
  geom_point(size = ifelse(select_usa_few$`Hannan-Quinn` > min(select_usa_few$`Hannan-Quinn`),1,3),
             color = ifelse(select_usa_few$`Hannan-Quinn` > min(select_usa_few$`Hannan-Quinn`),"black","red"),
             shape = ifelse(select_usa_few$`Hannan-Quinn` > min(select_usa_few$`Hannan-Quinn`),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4)))

select_usa_few

garchmodel_std[["USA_Index"]][["(2,2)"]]
garchmodel_std[["USA_Index"]][["(1,1)"]]

par(mfrow = c(2,2), mar = c(2,2,2,2))
for (i in c(10,11,7,3,1,2,8,9)){
  plot(garchmodel_std[["USA_Index"]][["(2,2)"]], which = i)
}

#ARG
select_arg_few <- head(select_arg[order(select_arg$BIC),],30)

p1 <- ggplot(select_arg_few,aes(x = AIC, y = rownames(select_arg_few))) +
  geom_point(size = ifelse(select_arg_few$AIC > min(select_arg_few$AIC),1,3),
             color = ifelse(select_arg_few$AIC > min(select_arg_few$AIC),"black","red"),
             shape = ifelse(select_arg_few$AIC > min(select_arg_few$AIC),19,8)) +
  ylab("Modelos") + theme(axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(margin= margin(1,1,3,3))) 
p2 <- ggplot(select_arg_few,aes(x = BIC, y = rownames(select_arg_few))) +
  geom_point(size = ifelse(select_arg_few$BIC > min(select_arg_few$BIC),1,3),
             color = ifelse(select_arg_few$BIC > min(select_arg_few$BIC),"black","red"),
             shape = ifelse(select_arg_few$BIC > min(select_arg_few$BIC),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p3 <- ggplot(select_arg_few,aes(x = Shibata, y = rownames(select_arg_few))) +
  geom_point(size = ifelse(select_arg_few$Shibata > min(select_arg_few$Shibata),1,3),
             color = ifelse(select_arg_few$Shibata > min(select_arg_few$Shibata),"black","red"),
             shape = ifelse(select_arg_few$Shibata > min(select_arg_few$Shibata),19,8)) +
  ylab("Modelos") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p4 <- ggplot(select_arg_few,aes(x = `Hannan-Quinn`, y = rownames(select_arg_few))) +
  geom_point(size = ifelse(select_arg_few$`Hannan-Quinn` > min(select_arg_few$`Hannan-Quinn`),1,3),
             color = ifelse(select_arg_few$`Hannan-Quinn` > min(select_arg_few$`Hannan-Quinn`),"black","red"),
             shape = ifelse(select_arg_few$`Hannan-Quinn` > min(select_arg_few$`Hannan-Quinn`),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4)))

select_arg_few

garchmodel_eskstd[["ARG_Price"]][["(4,4)"]]
garchmodel_eskstd[["ARG_Price"]][["(5,4)"]]
garchmodel_tstd[["ARG_Price"]][["(1,1)"]]


#EGPT
select_egpt_few <- head(select_egpt[order(select_egpt$BIC),],30)

p1 <- ggplot(select_egpt_few,aes(x = AIC, y = rownames(select_egpt_few))) +
  geom_point(size = ifelse(select_egpt_few$AIC > min(select_egpt_few$AIC),1,3),
             color = ifelse(select_egpt_few$AIC > min(select_egpt_few$AIC),"black","red"),
             shape = ifelse(select_egpt_few$AIC > min(select_egpt_few$AIC),19,8)) +
  ylab("Modelos") + theme(axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(margin= margin(1,1,3,3))) 
p2 <- ggplot(select_egpt_few,aes(x = BIC, y = rownames(select_egpt_few))) +
  geom_point(size = ifelse(select_egpt_few$BIC > min(select_egpt_few$BIC),1,3),
             color = ifelse(select_egpt_few$BIC > min(select_egpt_few$BIC),"black","red"),
             shape = ifelse(select_egpt_few$BIC > min(select_egpt_few$BIC),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p3 <- ggplot(select_egpt_few,aes(x = Shibata, y = rownames(select_egpt_few))) +
  geom_point(size = ifelse(select_egpt_few$Shibata > min(select_egpt_few$Shibata),1,3),
             color = ifelse(select_egpt_few$Shibata > min(select_egpt_few$Shibata),"black","red"),
             shape = ifelse(select_egpt_few$Shibata > min(select_egpt_few$Shibata),19,8)) +
  ylab("Modelos") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p4 <- ggplot(select_egpt_few,aes(x = `Hannan-Quinn`, y = rownames(select_egpt_few))) +
  geom_point(size = ifelse(select_egpt_few$`Hannan-Quinn` > min(select_egpt_few$`Hannan-Quinn`),1,3),
             color = ifelse(select_egpt_few$`Hannan-Quinn` > min(select_egpt_few$`Hannan-Quinn`),"black","red"),
             shape = ifelse(select_egpt_few$`Hannan-Quinn` > min(select_egpt_few$`Hannan-Quinn`),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4)))

select_egpt_few

garchmodel_estd[["EGPT_Price"]][["(4,3)"]]
garchmodel_estd[["EGPT_Price"]][["(4,5)"]]
par(mfrow = c(2,2))
for (i in c(10,11,7,3,1,2,8,9)){
  plot(garchmodel_estd[["EGPT_Price"]][["(4,3)"]] , which = i)
} 

#GHA
select_gha_few <- head(select_gha[order(select_gha$BIC),],30)

p1 <- ggplot(select_gha_few,aes(x = AIC, y = rownames(select_gha_few))) +
  geom_point(size = ifelse(select_gha_few$AIC > min(select_gha_few$AIC),1,3),
             color = ifelse(select_gha_few$AIC > min(select_gha_few$AIC),"black","red"),
             shape = ifelse(select_gha_few$AIC > min(select_gha_few$AIC),19,8)) +
  ylab("Modelos") + theme(axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(margin= margin(1,1,3,3))) 
p2 <- ggplot(select_gha_few,aes(x = BIC, y = rownames(select_gha_few))) +
  geom_point(size = ifelse(select_gha_few$BIC > min(select_gha_few$BIC),1,3),
             color = ifelse(select_gha_few$BIC > min(select_gha_few$BIC),"black","red"),
             shape = ifelse(select_gha_few$BIC > min(select_gha_few$BIC),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p3 <- ggplot(select_gha_few,aes(x = Shibata, y = rownames(select_gha_few))) +
  geom_point(size = ifelse(select_gha_few$Shibata > min(select_gha_few$Shibata),1,3),
             color = ifelse(select_gha_few$Shibata > min(select_gha_few$Shibata),"black","red"),
             shape = ifelse(select_gha_few$Shibata > min(select_gha_few$Shibata),19,8)) +
  ylab("Modelos") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p4 <- ggplot(select_gha_few,aes(x = `Hannan-Quinn`, y = rownames(select_gha_few))) +
  geom_point(size = ifelse(select_gha_few$`Hannan-Quinn` > min(select_gha_few$`Hannan-Quinn`),1,3),
             color = ifelse(select_gha_few$`Hannan-Quinn` > min(select_gha_few$`Hannan-Quinn`),"black","red"),
             shape = ifelse(select_gha_few$`Hannan-Quinn` > min(select_gha_few$`Hannan-Quinn`),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4)))

select_gha_few

garchmodel_estd[["GHA_Price"]][["(2,1)"]]
garchmodel_eskstd[["GHA_Price"]][["(4,5)"]]
garchmodel_tstd[["GHA_Price"]][["(1,1)"]]

#KEN
select_ken_few <- head(select_ken[order(select_ken$BIC),],30)

p1 <- ggplot(select_ken_few,aes(x = AIC, y = rownames(select_ken_few))) +
  geom_point(size = ifelse(select_ken_few$AIC > min(select_ken_few$AIC),1,3),
             color = ifelse(select_ken_few$AIC > min(select_ken_few$AIC),"black","red"),
             shape = ifelse(select_ken_few$AIC > min(select_ken_few$AIC),19,8)) +
  ylab("Modelos") + theme(axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(margin= margin(1,1,3,3))) 
p2 <- ggplot(select_ken_few,aes(x = BIC, y = rownames(select_ken_few))) +
  geom_point(size = ifelse(select_ken_few$BIC > min(select_ken_few$BIC),1,3),
             color = ifelse(select_ken_few$BIC > min(select_ken_few$BIC),"black","red"),
             shape = ifelse(select_ken_few$BIC > min(select_ken_few$BIC),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p3 <- ggplot(select_ken_few,aes(x = Shibata, y = rownames(select_ken_few))) +
  geom_point(size = ifelse(select_ken_few$Shibata > min(select_ken_few$Shibata),1,3),
             color = ifelse(select_ken_few$Shibata > min(select_ken_few$Shibata),"black","red"),
             shape = ifelse(select_ken_few$Shibata > min(select_ken_few$Shibata),19,8)) +
  ylab("Modelos") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p4 <- ggplot(select_ken_few,aes(x = `Hannan-Quinn`, y = rownames(select_ken_few))) +
  geom_point(size = ifelse(select_ken_few$`Hannan-Quinn` > min(select_ken_few$`Hannan-Quinn`),1,3),
             color = ifelse(select_ken_few$`Hannan-Quinn` > min(select_ken_few$`Hannan-Quinn`),"black","red"),
             shape = ifelse(select_ken_few$`Hannan-Quinn` > min(select_ken_few$`Hannan-Quinn`),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4)))

select_ken_few

#Test
garchmodel_std[["KEN_Price"]][["(1,1)"]]
garchmodel_tskstd[["KEN_Price"]][["(1,2)"]]
garchmodel_tstd[["KEN_Price"]][["(2,2)"]]
garchmodel_tstd[["KEN_Price"]][["(1,2)"]]


#MOR
select_mor_few <- head(select_mor[order(select_mor$BIC),],30)

p1 <- ggplot(select_mor_few,aes(x = AIC, y = rownames(select_mor_few))) +
  geom_point(size = ifelse(select_mor_few$AIC > min(select_mor_few$AIC),1,3),
             color = ifelse(select_mor_few$AIC > min(select_mor_few$AIC),"black","red"),
             shape = ifelse(select_mor_few$AIC > min(select_mor_few$AIC),19,8)) +
  ylab("Modelos") + theme(axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(margin= margin(1,1,3,3))) 
p2 <- ggplot(select_mor_few,aes(x = BIC, y = rownames(select_mor_few))) +
  geom_point(size = ifelse(select_mor_few$BIC > min(select_mor_few$BIC),1,3),
             color = ifelse(select_mor_few$BIC > min(select_mor_few$BIC),"black","red"),
             shape = ifelse(select_mor_few$BIC > min(select_mor_few$BIC),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p3 <- ggplot(select_mor_few,aes(x = Shibata, y = rownames(select_mor_few))) +
  geom_point(size = ifelse(select_mor_few$Shibata > min(select_mor_few$Shibata),1,3),
             color = ifelse(select_mor_few$Shibata > min(select_mor_few$Shibata),"black","red"),
             shape = ifelse(select_mor_few$Shibata > min(select_mor_few$Shibata),19,8)) +
  ylab("Modelos") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p4 <- ggplot(select_mor_few,aes(x = `Hannan-Quinn`, y = rownames(select_mor_few))) +
  geom_point(size = ifelse(select_mor_few$`Hannan-Quinn` > min(select_mor_few$`Hannan-Quinn`),1,3),
             color = ifelse(select_mor_few$`Hannan-Quinn` > min(select_mor_few$`Hannan-Quinn`),"black","red"),
             shape = ifelse(select_mor_few$`Hannan-Quinn` > min(select_mor_few$`Hannan-Quinn`),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4)))

select_mor_few

garchmodel_norm[["MOR_Price"]][["(1,1)"]]
garchmodel_std[["MOR_Price"]][["(1,1)"]]


#NIG
select_nig_few <- head(select_nig[order(select_nig$BIC),],30)

p1 <- ggplot(select_nig_few,aes(x = AIC, y = rownames(select_nig_few))) +
  geom_point(size = ifelse(select_nig_few$AIC > min(select_nig_few$AIC),1,3),
             color = ifelse(select_nig_few$AIC > min(select_nig_few$AIC),"black","red"),
             shape = ifelse(select_nig_few$AIC > min(select_nig_few$AIC),19,8)) +
  ylab("Modelos") + theme(axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(margin= margin(1,1,3,3))) 
p2 <- ggplot(select_nig_few,aes(x = BIC, y = rownames(select_nig_few))) +
  geom_point(size = ifelse(select_nig_few$BIC > min(select_nig_few$BIC),1,3),
             color = ifelse(select_nig_few$BIC > min(select_nig_few$BIC),"black","red"),
             shape = ifelse(select_nig_few$BIC > min(select_nig_few$BIC),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p3 <- ggplot(select_nig_few,aes(x = Shibata, y = rownames(select_nig_few))) +
  geom_point(size = ifelse(select_nig_few$Shibata > min(select_nig_few$Shibata),1,3),
             color = ifelse(select_nig_few$Shibata > min(select_nig_few$Shibata),"black","red"),
             shape = ifelse(select_nig_few$Shibata > min(select_nig_few$Shibata),19,8)) +
  ylab("Modelos") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p4 <- ggplot(select_nig_few,aes(x = `Hannan-Quinn`, y = rownames(select_nig_few))) +
  geom_point(size = ifelse(select_nig_few$`Hannan-Quinn` > min(select_nig_few$`Hannan-Quinn`),1,3),
             color = ifelse(select_nig_few$`Hannan-Quinn` > min(select_nig_few$`Hannan-Quinn`),"black","red"),
             shape = ifelse(select_nig_few$`Hannan-Quinn` > min(select_nig_few$`Hannan-Quinn`),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4)))

select_nig_few

garchmodel_estd[["NIG_Price"]][["(2,1)"]]
garchmodel_eskstd[["NIG_Price"]][["(4,3)"]]
garchmodel_estd[["NIG_Price"]][["(1,1)"]]

par(mfrow = c(2,2))
for (i in c(10,11,7,3,1,2,8,9)){
  plot(garchmodel_estd[["NIG_Price"]][["(1,1)"]] , which = i)
} 
par(mfrow = c(1,1))


#SOUTHAFR
select_southafr_few <- head(select_southafr[order(select_southafr$BIC),],30)

p1 <- ggplot(select_southafr_few,aes(x = AIC, y = rownames(select_southafr_few))) +
  geom_point(size = ifelse(select_southafr_few$AIC > min(select_southafr_few$AIC),1,3),
             color = ifelse(select_southafr_few$AIC > min(select_southafr_few$AIC),"black","red"),
             shape = ifelse(select_southafr_few$AIC > min(select_southafr_few$AIC),19,8)) +
  ylab("Modelos") + theme(axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(margin= margin(1,1,3,3))) 
p2 <- ggplot(select_southafr_few,aes(x = BIC, y = rownames(select_southafr_few))) +
  geom_point(size = ifelse(select_southafr_few$BIC > min(select_southafr_few$BIC),1,3),
             color = ifelse(select_southafr_few$BIC > min(select_southafr_few$BIC),"black","red"),
             shape = ifelse(select_southafr_few$BIC > min(select_southafr_few$BIC),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p3 <- ggplot(select_southafr_few,aes(x = Shibata, y = rownames(select_southafr_few))) +
  geom_point(size = ifelse(select_southafr_few$Shibata > min(select_southafr_few$Shibata),1,3),
             color = ifelse(select_southafr_few$Shibata > min(select_southafr_few$Shibata),"black","red"),
             shape = ifelse(select_southafr_few$Shibata > min(select_southafr_few$Shibata),19,8)) +
  ylab("Modelos") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p4 <- ggplot(select_southafr_few,aes(x = `Hannan-Quinn`, y = rownames(select_southafr_few))) +
  geom_point(size = ifelse(select_southafr_few$`Hannan-Quinn` > min(select_southafr_few$`Hannan-Quinn`),1,3),
             color = ifelse(select_southafr_few$`Hannan-Quinn` > min(select_southafr_few$`Hannan-Quinn`),"black","red"),
             shape = ifelse(select_southafr_few$`Hannan-Quinn` > min(select_southafr_few$`Hannan-Quinn`),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4)))

select_southafr_few

garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]
garchmodel_naskstd[["SOUTHAFR_Price"]][["(1,4)"]]
garchmodel_naskstd[["SOUTHAFR_Price"]][["(1,1)"]]

#TAN
select_tan_few <- head(select_tan[order( select_tan$BIC),],30)

p1 <- ggplot(select_tan_few,aes(x = AIC, y = rownames(select_tan_few))) +
  geom_point(size = ifelse(select_tan_few$AIC > min(select_tan_few$AIC),1,3),
             color = ifelse(select_tan_few$AIC > min(select_tan_few$AIC),"black","red"),
             shape = ifelse(select_tan_few$AIC > min(select_tan_few$AIC),19,8)) +
  ylab("Modelos") + theme(axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(margin= margin(1,1,3,3))) 
p2 <- ggplot(select_tan_few,aes(x = BIC, y = rownames(select_tan_few))) +
  geom_point(size = ifelse(select_tan_few$BIC > min(select_tan_few$BIC),1,3),
             color = ifelse(select_tan_few$BIC > min(select_tan_few$BIC),"black","red"),
             shape = ifelse(select_tan_few$BIC > min(select_tan_few$BIC),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p3 <- ggplot(select_tan_few,aes(x = Shibata, y = rownames(select_tan_few))) +
  geom_point(size = ifelse(select_tan_few$Shibata > min(select_tan_few$Shibata),1,3),
             color = ifelse(select_tan_few$Shibata > min(select_tan_few$Shibata),"black","red"),
             shape = ifelse(select_tan_few$Shibata > min(select_tan_few$Shibata),19,8)) +
  ylab("Modelos") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p4 <- ggplot(select_tan_few,aes(x = `Hannan-Quinn`, y = rownames(select_tan_few))) +
  geom_point(size = ifelse(select_tan_few$`Hannan-Quinn` > min(select_tan_few$`Hannan-Quinn`),1,3),
             color = ifelse(select_tan_few$`Hannan-Quinn` > min(select_tan_few$`Hannan-Quinn`),"black","red"),
             shape = ifelse(select_tan_few$`Hannan-Quinn` > min(select_tan_few$`Hannan-Quinn`),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4)))

select_tan_few

garchmodel_estd[["TAN_Price"]][["(1,2)"]]
garchmodel_estd[["TAN_Price"]][["(2,3)"]]
garchmodel_estd[["TAN_Price"]][["(1,1)"]]


#TUN
select_tun_few <- head(select_tun[order(select_tun$BIC),],30)

p1 <- ggplot(select_tun_few,aes(x = AIC, y = rownames(select_tun_few))) +
  geom_point(size = ifelse(select_tun_few$AIC > min(select_tun_few$AIC),1,3),
             color = ifelse(select_tun_few$AIC > min(select_tun_few$AIC),"black","red"),
             shape = ifelse(select_tun_few$AIC > min(select_tun_few$AIC),19,8)) +
  ylab("Modelos") + theme(axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(margin= margin(1,1,3,3))) 
p2 <- ggplot(select_tun_few,aes(x = BIC, y = rownames(select_tun_few))) +
  geom_point(size = ifelse(select_tun_few$BIC > min(select_tun_few$BIC),1,3),
             color = ifelse(select_tun_few$BIC > min(select_tun_few$BIC),"black","red"),
             shape = ifelse(select_tun_few$BIC > min(select_tun_few$BIC),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p3 <- ggplot(select_tun_few,aes(x = Shibata, y = rownames(select_tun_few))) +
  geom_point(size = ifelse(select_tun_few$Shibata > min(select_tun_few$Shibata),1,3),
             color = ifelse(select_tun_few$Shibata > min(select_tun_few$Shibata),"black","red"),
             shape = ifelse(select_tun_few$Shibata > min(select_tun_few$Shibata),19,8)) +
  ylab("Modelos") + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p4 <- ggplot(select_tun_few,aes(x = `Hannan-Quinn`, y = rownames(select_tun_few))) +
  geom_point(size = ifelse(select_tun_few$`Hannan-Quinn` > min(select_tun_few$`Hannan-Quinn`),1,3),
             color = ifelse(select_tun_few$`Hannan-Quinn` > min(select_tun_few$`Hannan-Quinn`),"black","red"),
             shape = ifelse(select_tun_few$`Hannan-Quinn` > min(select_tun_few$`Hannan-Quinn`),19,8)) +
  ylab("Modelos") + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4)))

select_tun_few

garchmodel_std[["TUN_Price"]][["(1,1)"]]
garchmodel_eskstd[["TUN_Price"]][["(2,3)"]]

resumenselectdiagindv <- rbind(select_usa["SGARCH T-Student USA_Index (2,2)",],
                               select_arg["TGARCH T-Student ARG_Price (1,1)",],
                               select_egpt["EGARCH T-Student EGPT_Price (4,3)",],
                               select_gha["EGARCH T-Student GHA_Price (2,1)",],
                               select_ken["SGARCH T-Student KEN_Price (1,1)",],
                               select_mor["SGARCH Norm MOR_Price (1,1)",],
                               select_nig["EGARCH T-Student NIG_Price (1,1)",],
                               select_southafr["SGARCH Skewed T-Student SOUTHAFR_Price (1,1)",],
                               select_tan["EGARCH T-Student TAN_Price (1,1)",],
                               select_tun["SGARCH T-Student TUN_Price (1,1)",])


#RESUMEN GENERAL
resumenindv <- data.frame()
resumenindv <- data.frame(rownames(resumenselectdefectindv))
resumenindv <- cbind(resumenindv,data.frame(rownames(resumenselectvindv)))
resumenindv <- cbind(resumenindv,data.frame(rownames(resumenselecthindv)))
resumenindv <- cbind(resumenindv,data.frame(rownames(resumenselectdiagindv)))

colnames(resumenindv) <- c("Selección por defecto","Selección Vertical: Fijando orden (1,1)",
                           "Selección Horizontal: Fijando modelo","Selección Diagonal")

