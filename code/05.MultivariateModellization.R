#rm(list = ls())
#setwd()

#################################### LIBRERÍAS #################################
library(rmgarch)
library(rugarch)
library(ggplot)
library(glue)
library(RiskPortfolios)
library(fBasics)
library(MVN)
library(portes)
######################### MODELIZACIÓN MULTIVARIANTE ###########################
load("rend_zoo.rda")
names <- colnames(rend_zoo)
############## MEQMA: MULTIVARIATE EQUALLY WEIGHTED MOVING AVERAGE #############

# All sample
time <- index(rend_zoo)
cor_meqma_all <- array(rep(NA, 10*10*1303), dim = c(10, 10, 1303))
cor_meqma_all_list <- list()
definitepost_meqma <- list()
for (i in seq_along(time)[2:1303]){ 
  sample <- window(rend_zoo, start = time[1], end = time[i] )
  cor_meqma_all_list[[i]] <- cov2cor(covEstimation(sample, 
                                                   control = list(type = 'naive')))
  cor_meqma_all[,, i] <- cor_meqma_all_list[[i]]
  
  definitepost_meqma[[i]]<-isPositiveDefinite(covEstimation(sample, 
                                                            control = list(type = 'naive')))
}

dimnames(cor_meqma_all) <- list(names[1:10], names[1:10], as.character(time[1:1303]))
cor_meqma_all

definitepost_meqma

cov_muestral_meqma <- covEstimation(rend_zoo, control = list(type = 'naive'))
cov_muestral_meqma
cor_muestral_meqma <- cov2cor(cov_muestral_meqma)
cor_muestral_meqma
cor_meqma_all[,,"2005-01-04"] <- cor_muestral_meqma 

isPositiveDefinite(cov_muestral_meqma) 

#Gráfico de la correlación
#Sin eliminar las 10 primeras
par(mfrow = c(3,1))
par(mar =c(3,3,3,3))
for (i in names[2:10]){
  plot(as.timeSeries(cor_meqma_all["USA_Index",i,]), lwd = 1.8,
       main = glue("Correlación condicional USA_Index y {i}"))
}
par(mfrow = c(1,1))

#Eliminando las 10 primeras
par(mfrow = c(3,1), mar =c(2,2,2,2), cex=1)
for (i in names[2:10]){
  plot(as.timeSeries(cor_meqma_all["USA_Index",i,10:1303]), lwd = 1.8,
       main = glue("Correlación condicional USA_Index y {i}"))
}
par(mfrow = c(1,1))

############# MEWMA: MULTIVARIATE EXPONTENTIALLY MOVING AVERAGE ################

# All sample
time <- index(rend_zoo)
cor_mewma_all <- array(rep(NA, 10*10*1303), dim = c(10, 10, 1303))
cor_mewma_all_list <- list()
definitepost_mewma <- list()
for (i in seq_along(time)[2:1303]){ # debería eliminar la primera observación??
  sample <- window(rend_zoo, start = time[1], end = time[i])
  cor_mewma_all_list[[i]] <- cov2cor(covEstimation(sample,
                                                   control = list(type = 'ewma')))
  cor_mewma_all[,, i] <- cor_mewma_all_list[[i]]
  
  definitepost_mewma[[i]]<-isPositiveDefinite(covEstimation(sample, 
                                                            control = list(type = 'naive')))
}

dimnames(cor_mewma_all) <- list(names[1:10], names[1:10], as.character(time[1:1303]))
cor_mewma_all

definitepost_mewma

cov_muestral_mewma <- covEstimation(rend_zoo, control = list(type = 'ewma'))
cov_muestral_mewma
cor_muestral_mewma <- cov2cor(cov_muestral_mewma)
cor_muestral_mewma
cor_mewma_all[,,"2005-01-04"] <- cor_muestral_mewma

isPositiveDefinite(cov_muestral_mewma)

#Gráfico de la correlación
#Sin eliminar las 10 primeras
par(mfrow = c(3,1))
par(mar =c(3,3,3,3))
for (i in names[2:10]){
  plot(as.timeSeries(cor_mewma_all["USA_Index",i,]), lwd = 1.8,
       main = glue("Correlación condicional USA_Index y {i}"))
}
par(mfrow = c(1,1))

#Eliminando las 10 primeras
par(mfrow = c(3,1), mar =c(2,2,2,2), cex=1)
for (i in names[2:10]){
  plot(as.timeSeries(cor_mewma_all["USA_Index",i,10:1303]), lwd = 1.8,
       main = glue("Correlación condicional USA_Index y {i}"))
}
par(mfrow = c(1,1))
#Distorsiona menos y los graficos son más claros


############ DCC GARCH: DYNAMIC CONDITIONAL CORRELATION GARCH MODEL ############

#Pre-Diagnosis:
#Test de Normalidad Multivariante de Henze-Zirkler
mvntest <- mvn(data = rend_zoo, mvnTest = "hz")
mvntest$multivariateNormality 

# Test T-Student Multivariante de Mardia
mvntest <- mvn(data = rend_zoo, mvnTest = "mardia")
mvntest$multivariateNormality 

#----------------- DCC SIMÉTRICO CON SELECCIÓN UNIV POR DEFECTO ---------------

#DCC SIMÉTRICO NORMAL, UNIV POR DEFECTO
specdefec <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                        distribution.model="norm")

multispec <- multispec(replicate(10,specdefec))

multift <- multifit(multispec,rend_zoo)

#Modelo básico: DCC GARCH NORMAL (1,1)
dccspecdefec <- dccspec(uspec = multispec, dccOrder= c(1,1), model = c("DCC"), 
                        distribution = "mvnorm")
dccfitdefec <- dccfit(dccspecdefec, data = rend_zoo, 
                      fit.control =  list(eval.se = TRUE),fit = multift,
                      solver="solnp", solver.control = list(tol = 1e-12))
dccfitdefec

rcor(dccfitdefec, type="R")#[,,'2005-10-17'] #array of corr
rcov(dccfitdefec)


#DIAGNOSIS

#Gráficos generales
#¿Son definidas positivas?
for (i in seq_along(time)){
  print(isPositiveDefinite(rcov(dccfitdefec)[,,i]))
} #son todas definidas positivas

#Marginal Standarized Residuals
par(mfrow = c(4,1), mar =c(2,2,2,2))
for (i in seq_along(names)){
  plot(as.timeSeries(dccfitdefec@mfit$stdresid[,i]),lwd = 1.8,
       main = glue("Standarized Residuals {names[i]}"))
}
par(mfrow = c(1,1))

#Square Residuals
par(mfrow = c(4,1), mar =c(2,2,2,2))
for (i in seq_along(names)){
  plot(as.timeSeries(dccfitdefec@mfit$stdresid[,i]^2), lwd = 1.8,
       main = glue("Square Standarized Residuals {names[i]}"))
}
par(mfrow = c(1,1))


#Coeficientes
dccfitdefec

#Estimated volatility vs realized volatility (abs returns)
par(mar=c(2,2,4,2))
plot(dccfitdefec, which=2, series=c(1,2))
plot(dccfitdefec, which=2, series=c(3,4))
plot(dccfitdefec, which=2, series=c(5,6))
plot(dccfitdefec, which=2, series=c(7,8))
plot(dccfitdefec, which=2, series=c(9,10))

#Time varianting correlation among series with USA
par(mfrow = c(3,1), mar =c(2,2,2,2), cex=1)
for (i in names[2:10]){
  plot(as.timeSeries(rcor(dccfitdefec, type="R")["USA_Index",i,]), lwd = 1.8,
       main = glue("Correlación condicional USA_Index y {i}"))
}
par(mfrow = c(1,1))

#Time varianting covariance among series with USA
par(mfrow = c(3,1), mar =c(3,3,3,3))
for (i in seq_along(names)){
  plot(as.timeSeries(rcov(dccfitdefec)["USA_Index",i,], type = "l"), lwd = 1.8,
       main = glue("Covarianza condicional {names[i]}"))
}
par(mfrow = c(1,1))

#Comparación ACF para los residuos estandarizados univariantes ajustado frente a
#los residuos estandarizados marginales del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4))
for(i in names){
  acf(garchmodel_norm[[i]][["(1,1)"]]@fit[["z"]], 
      main = glue("ACF of Standarized Residuals {i} GARCH (1,1)"))
  acf(dccfitdefec@mfit$stdresid[,match(glue("{i}"),names)], 
      main = glue("ACF of Marginal Standarized Residuals {i}"))
}
par(mfrow = c(1,1))

#Comparación ACF para los residuos estandarizados al cuadrado univariantes ajustado 
#frente a los residuos estandarizados marginales al cuadrado del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4))
for(i in names){
  acf(garchmodel_norm[[i]][["(1,1)"]]@fit[["z"]]^2, 
      main = glue("ACF of Standarized Squared Residuals {i} GARCH (1,1)"))
  acf(dccfitdefec@mfit$stdresid[,match(glue("{i}"),names)]^2, 
      main = glue("ACF of Marginal Standarized Squared Residuals {i}"))
}
par(mfrow = c(1,1))

#Comparación PACF para los residuos estandarizados univariantes ajustado frente a
#los residuos estandarizados marginales del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4))
for(i in names){
  pacf(garchmodel_norm[[i]][["(1,1)"]]@fit[["z"]], 
       main = glue("PACF of Standarized Residuals {i} GARCH (1,1)"))
  pacf(dccfitdefec@mfit$stdresid[,match(glue("{i}"),names)], 
       main = glue("PACF of Marginal Standarized Residuals {i}"))
}
par(mfrow = c(1,1))

#Comparación PACF para los residuos estandarizados al cuadrado univariantes ajustado 
#frente a los residuos estandarizados marginales al cuadrado del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4))
for(i in names){
  pacf(garchmodel_norm[[i]][["(1,1)"]]@fit[["z"]]^2, 
       main = glue("PACF of Standarized Squared Residuals {i} GARCH (1,1)"))
  pacf(dccfitdefec@mfit$stdresid[,match(glue("{i}"),names)]^2, 
       main = glue("PACF of Marginal Standarized Squared Residuals {i}"))
}
par(mfrow=c(1,1))

#Multivariate Portmanteu-Test (Ljung-Box test for multivariate)
#H0: no hay autocorrelación
LjungBox(dccfitdefec@mfit$stdresid,lags = (1:20),squared.residuals = FALSE)

#Multivariate Portmanteu-Test, squared.residuals = TRUE (ARCH EFFECTS)
#H0: no ARCH effect
LjungBox(dccfitdefec@mfit$stdresid^2,lags = (1:20),squared.residuals = TRUE)

#CCF graph
#Estamos viendo si existe autocorrelación de los residuos de las series con USA
par(mfrow=c(3,1),mar=c(3,3,3,3),cex.main=1.5)
for (i in 2:10){
  ccf(dccfitdefec@mfit$stdresid[,1],dccfitdefec@mfit$stdresid[,i],
      main = glue("Cross Correlation Standarized Residuals of {names[i]} with USA_Index"))
  title(line=0.5)
}

#----------------- DCC SIMÉTRICO T-STUDENT CON SELECCIÓN UNIV VERTICAL ---------

#Recordamos que los modelos son:
resumenselectvindv

#DCC SIMÉTRICO T-STUDENT
specvusa <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(2,2)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="std")

specvarg <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="std")

specvegpt <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(4,3)),
                        mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                        distribution.model="std")

specvgha <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,2)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="std")

specvken <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="std")

specvmor <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="snorm")

specvnig <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="std")

specvsouthafr <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="sstd")

specvtan <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,2)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="std")

specvtun <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="norm")

multispec <- multispec(c(specvusa,specvarg,specvegpt,specvgha,specvken,specvmor,
                         specvnig,specvsouthafr,specvtan,specvtun))

multift <- multifit(multispec,rend_zoo)

#Modelo básico: DCC GARCH T-STUDENT (1,1)
dccspecv <- dccspec(uspec = multispec, dccOrder= c(1,1), model = c("DCC"), 
                    distribution = "mvt")
dccfitv <- dccfit(dccspecv, data = rend_zoo, 
                  fit.control =  list(eval.se = TRUE),fit = multift,
                  solver="solnp", solver.control = list(tol = 1e-12))
dccfitv

rcor(dccfitv, type="R")#[,,'2005-10-17'] #array of corr
rcov(dccfitv)


#DIAGNOSIS

#Gráficos generales
#¿Son definidas positivas?
for (i in seq_along(time)){
  print(isPositiveDefinite(rcov(dccfitv)[,,i]))
} 

#Marginal Standarized Residuals
par(mfrow = c(4,1), mar =c(2,2,2,2))
for (i in seq_along(names)){
  plot(as.timeSeries(dccfitv@mfit$stdresid[,i]),lwd = 1.8,
       main = glue("Standarized Residuals {names[i]}"))
}
par(mfrow = c(1,1))

#Square Residuals
par(mfrow = c(4,1), mar =c(2,2,2,2))
for (i in seq_along(names)){
  plot(as.timeSeries(dccfitv@mfit$stdresid[,i]^2), lwd = 1.8,
       main = glue("Square Standarized Residuals {names[i]}"))
}
par(mfrow = c(1,1))

#Coeficientes
dccfitv

#Estimated volatility vs realized volatility (abs returns)
par(mar=c(2,2,4,2))
plot(dccfitv, which=2, series=c(1,2))
plot(dccfitv, which=2, series=c(3,4))
plot(dccfitv, which=2, series=c(5,6))
plot(dccfitv, which=2, series=c(7,8))
plot(dccfitv, which=2, series=c(9,10))

#Time varianting correlation among series with USA
par(mfrow = c(3,1), mar =c(2,2,2,2), cex=1)
for (i in names[2:10]){
  plot(as.timeSeries(rcor(dccfitv, type="R")["USA_Index",i,]), lwd = 1.8,
       main = glue("Correlación condicional USA_Index y {i}"))
}
par(mfrow = c(1,1))

#Time varianting covariance among series with USA
par(mfrow = c(3,1), mar =c(3,3,3,3))
for (i in seq_along(names)){
  plot(as.timeSeries(rcov(dccfitv)["USA_Index",i,], type = "l"), lwd = 1.8,
       main = glue("Covarianza condicional {names[i]}"))
}
par(mfrow = c(1,1))

#Comparación ACF para los residuos estandarizados univariantes ajustado frente a
#los residuos estandarizados marginales del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75, cex.main = 1.4)
acf(garchmodel_estd[["USA_Index"]][["(2,2)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals USA_Index EGARCH T-Student (2,2)"))
acf(dccfitv@mfit$stdresid[,match("USA_Index",names)], 
    main = glue("ACF of Marginal Standarized Residuals USA_Index"))

acf(garchmodel_std[["ARG_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals ARG_Price SGARCH T-Student (1,1)"))
acf(dccfitv@mfit$stdresid[,match("ARG_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals ARG_Price"))

acf(garchmodel_estd[["EGPT_Price"]][["(4,3)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals EGPT_Price EGARCH T-Student (4,3)"))
acf(dccfitv@mfit$stdresid[,match("EGPT_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals EGPT_Price"))

acf(garchmodel_estd[["GHA_Price"]][["(1,2)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals GHA_Price EGARCH T-Student (1,2)"))
acf(dccfitv@mfit$stdresid[,match("GHA_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals GHA_Price"))

acf(garchmodel_std[["KEN_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals KEN_Price SGARCH T-Student (1,1)"))
acf(dccfitv@mfit$stdresid[,match("KEN_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals KEN_Price"))

acf(garchmodel_sknorm[["MOR_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals MOR_Price SGARCH Skewed Normal (1,1)"))
acf(dccfitv@mfit$stdresid[,match("MOR_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals MOR_Price"))

acf(garchmodel_estd[["NIG_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals NIG_Price EGARCH T-Student (1,1)"))
acf(dccfitv@mfit$stdresid[,match("NIG_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals NIG_Price"))

acf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfitv@mfit$stdresid[,match("SOUTHAFR_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals SOUTHAFR_Price"))

acf(garchmodel_skstd[["TAN_Price"]][["(1,2)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals TAN_Price EGARCH T-Student (1,2)"))
acf(dccfitv@mfit$stdresid[,match("TAN_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals TAN_Price"))

acf(garchmodel_norm[["TUN_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals TUN_Price SGARCH Normal (1,1)"))
acf(dccfitv@mfit$stdresid[,match("TUN_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals TUN_Price"))

par(mfrow = c(1,1))

#MOR y TUN peor que la univariante
#solo bien south y tan, el resto iguañ

#Comparación ACF para los residuos estandarizados al cuadrado univariantes ajustado 
#frente a los residuos estandarizados marginales al cuadrado del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75, cex.main = 1.4)
acf(garchmodel_estd[["USA_Index"]][["(2,2)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals USA_Index EGARCH T-Student (2,2)"))
acf(dccfitv@mfit$stdresid[,match("USA_Index",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals USA_Index"))

acf(garchmodel_std[["ARG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals ARG_Price SGARCH T-Student (1,1)"))
acf(dccfitv@mfit$stdresid[,match("ARG_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals ARG_Price"))

acf(garchmodel_estd[["EGPT_Price"]][["(4,3)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals EGPT_Price EGARCH T-Student (4,3)"))
acf(dccfitv@mfit$stdresid[,match("EGPT_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals EGPT_Price"))

acf(garchmodel_estd[["GHA_Price"]][["(1,2)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals GHA_Price EGARCH T-Student (1,2)"))
acf(dccfitv@mfit$stdresid[,match("GHA_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals GHA_Price"))

acf(garchmodel_std[["KEN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals KEN_Price SGARCH T-Student (1,1)"))
acf(dccfitv@mfit$stdresid[,match("KEN_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals KEN_Price"))

acf(garchmodel_sknorm[["MOR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals MOR_Price SGARCH Skewed Normal (1,1)"))
acf(dccfitv@mfit$stdresid[,match("MOR_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals MOR_Price"))

acf(garchmodel_estd[["NIG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals NIG_Price EGARCH T-Student (1,1)"))
acf(dccfitv@mfit$stdresid[,match("NIG_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals NIG_Price"))

acf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfitv@mfit$stdresid[,match("SOUTHAFR_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals SOUTHAFR_Price"))

acf(garchmodel_skstd[["TAN_Price"]][["(1,2)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals TAN_Price EGARCH T-Student (1,2)"))
acf(dccfitv@mfit$stdresid[,match("TAN_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals TAN_Price"))

acf(garchmodel_norm[["TUN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals TUN_Price SGARCH Normal (1,1)"))
acf(dccfitv@mfit$stdresid[,match("TUN_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals TUN_Price"))

par(mfrow = c(1,1))

#USA,MOR y TUN autocorrelación, el resto igual que su marginal

#Comparación PACF para los residuos estandarizados univariantes ajustado frente a
#los residuos estandarizados marginales del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75, cex.main = 1.4)
pacf(garchmodel_estd[["USA_Index"]][["(2,2)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals USA_Index EGARCH T-Student (2,2)"))
pacf(dccfitv@mfit$stdresid[,match("USA_Index",names)], 
     main = glue("PACF of Marginal Standarized Residuals USA_Index"))

pacf(garchmodel_std[["ARG_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals ARG_Price SGARCH T-Student (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("ARG_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals ARG_Price"))

pacf(garchmodel_estd[["EGPT_Price"]][["(4,3)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals EGPT_Price EGARCH T-Student (4,3)"))
pacf(dccfitv@mfit$stdresid[,match("EGPT_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals EGPT_Price"))

pacf(garchmodel_estd[["GHA_Price"]][["(1,2)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals GHA_Price EGARCH T-Student (1,2)"))
pacf(dccfitv@mfit$stdresid[,match("GHA_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals GHA_Price"))

pacf(garchmodel_std[["KEN_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals KEN_Price SGARCH T-Student (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("KEN_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals KEN_Price"))

pacf(garchmodel_sknorm[["MOR_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals MOR_Price SGARCH Skewed Normal (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("MOR_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals MOR_Price"))

pacf(garchmodel_estd[["NIG_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals NIG_Price EGARCH T-Student (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("NIG_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals NIG_Price"))

pacf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("SOUTHAFR_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals SOUTHAFR_Price"))

pacf(garchmodel_skstd[["TAN_Price"]][["(1,2)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals TAN_Price EGARCH T-Student (1,2)"))
pacf(dccfitv@mfit$stdresid[,match("TAN_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals TAN_Price"))

pacf(garchmodel_norm[["TUN_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("ACF of Standarized Residuals TUN_Price SGARCH Normal (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("TUN_Price",names)], 
     main = glue("ACF of Marginal Standarized Residuals TUN_Price"))

par(mfrow = c(1,1))

#Comparación PACF para los residuos estandarizados al cuadrado univariantes ajustado 
#frente a los residuos estandarizados marginales al cuadrado del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75, cex.main = 1.4)
pacf(garchmodel_estd[["USA_Index"]][["(2,2)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals USA_Index EGARCH T-Student (2,2)"))
pacf(dccfitv@mfit$stdresid[,match("USA_Index",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals USA_Index"))

pacf(garchmodel_std[["ARG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals ARG_Price SGARCH T-Student (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("ARG_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals ARG_Price"))

pacf(garchmodel_estd[["EGPT_Price"]][["(4,3)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals EGPT_Price EGARCH T-Student (4,3)"))
pacf(dccfitv@mfit$stdresid[,match("EGPT_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals EGPT_Price"))

pacf(garchmodel_estd[["GHA_Price"]][["(1,2)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals GHA_Price EGARCH T-Student (1,2)"))
pacf(dccfitv@mfit$stdresid[,match("GHA_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals GHA_Price"))

pacf(garchmodel_std[["KEN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals KEN_Price SGARCH T-Student (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("KEN_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals KEN_Price"))

pacf(garchmodel_sknorm[["MOR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals MOR_Price SGARCH Skewed Normal (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("MOR_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals MOR_Price"))

pacf(garchmodel_estd[["NIG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals NIG_Price EGARCH T-Student (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("NIG_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals NIG_Price"))

pacf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("SOUTHAFR_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals SOUTHAFR_Price"))

pacf(garchmodel_skstd[["TAN_Price"]][["(1,2)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals TAN_Price EGARCH T-Student (1,2)"))
pacf(dccfitv@mfit$stdresid[,match("TAN_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals TAN_Price"))

pacf(garchmodel_norm[["TUN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals TUN_Price SGARCH Normal (1,1)"))
pacf(dccfitv@mfit$stdresid[,match("TUN_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals TUN_Price"))

par(mfrow = c(1,1))

#Multivariate Portmanteu-Test (Ljung-Box test for multivariate)
#H0: no hay autocorrelación
LjungBox(dccfitv@mfit$stdresid,lags = (1:20),squared.residuals = FALSE)

#Multivariate Portmanteu-Test, squared.residuals = TRUE (ARCH EFFECTS)
#H0: no ARCH effect
LjungBox(dccfitv@mfit$stdresid^2,lags = (1:20),squared.residuals = TRUE)

#CCF graph
#Estamos viendo si existe autocorrelación de los residuos de las series con USA
par(mfrow=c(3,1),mar=c(3,3,3,3), cex.main =1.5)
for (i in 2:10){
  ccf(dccfitv@mfit$stdresid[,1],dccfitdefec@mfit$stdresid[,i],
      main = glue("Cross Correlation Standarized Residuals of {names[i]} with USA_Index"))
}

#---------------- DCC SIMÉTRICO T-STUDENT CON SELECCIÓN UNIV HORIZONTAL --------

#Recordamos que los modelos son:
resumenselecthindv

#DCC SIMÉTRICO T-STUDENT
spechusa <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(2,2)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="sstd")

specharg <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="std")

spechegpt <- ugarchspec(variance.model=list(model="fGARCH", submodel = "TGARCH",
                                            garchOrder=c(2,2)),
                        mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                        distribution.model="std")

spechgha <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="sstd")

spechken <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="sstd")

spechmor <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="std")

spechnig <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="sstd")

spechsouthafr <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="sstd")

spechtan <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="std")

spechtun <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="std")

multispec <- multispec(c(spechusa,specharg,spechegpt,spechgha,spechken,spechmor,
                         spechnig,spechsouthafr,spechtan,spechtun))

multift <- multifit(multispec,rend_zoo)

#Modelo básico: DCC GARCH T-STUDENT (1,1)
dccspech <- dccspec(uspec = multispec, dccOrder= c(1,1), model = c("DCC"), 
                    distribution = "mvt")
dccfith <- dccfit(dccspech, data = rend_zoo, 
                  fit.control =  list(eval.se = TRUE),fit = multift,
                  solver="solnp", solver.control = list(tol = 1e-12))
dccfith

rcor(dccfith, type="R")#[,,'2005-10-17'] #array of corr
rcov(dccfith)


#DIAGNOSIS

#Gráficos generales
#¿Son definidas positivas?
for (i in seq_along(time)){
  print(isPositiveDefinite(rcov(dccfith)[,,i]))
} 

#Marginal Standarized Residuals
par(mfrow = c(4,1), mar =c(2,2,2,2))
for (i in seq_along(names)){
  plot(as.timeSeries(dccfith@mfit$stdresid[,i]),lwd = 1.8,
       main = glue("Standarized Residuals {names[i]}"))
}
par(mfrow = c(1,1))

#Square Residuals
par(mfrow = c(4,1), mar =c(2,2,2,2))
for (i in seq_along(names)){
  plot(as.timeSeries(dccfith@mfit$stdresid[,i]^2), lwd = 1.8,
       main = glue("Square Standarized Residuals {names[i]}"))
}
par(mfrow = c(1,1))


#Coeficientes
dccfith

#Estimated volatility vs realized volatility (abs returns)
par(mar=c(2,2,4,2))
plot(dccfith, which=2, series=c(1,2))
plot(dccfith, which=2, series=c(3,4))
plot(dccfith, which=2, series=c(5,6))
plot(dccfith, which=2, series=c(7,8))
plot(dccfith, which=2, series=c(9,10))

#Time varianting correlation among series with USA
par(mfrow = c(3,1), mar =c(2,2,2,2),cex=1)
for (i in names[2:10]){
  plot(as.timeSeries(rcor(dccfith, type="R")["USA_Index",i,]), lwd = 1.8,
       main = glue("Correlación condicional USA_Index y {i}"))
}
par(mfrow = c(1,1))

#Time varianting covariance among series with USA
par(mfrow = c(3,1), mar =c(3,3,3,3))
for (i in seq_along(names)){
  plot(as.timeSeries(rcov(dccfith)["USA_Index",i,], type = "l"), lwd = 1.8,
       main = glue("Covarianza condicional {names[i]}"))
}
par(mfrow = c(1,1))

#Comparación ACF para los residuos estandarizados univariantes ajustado frente a
#los residuos estandarizados marginales del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75, cex.main = 1.2)
acf(garchmodel_eskstd[["USA_Index"]][["(2,2)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals USA_Index EGARCH Skewed T-Student (2,2)"))
acf(dccfith@mfit$stdresid[,match("USA_Index",names)], 
    main = glue("ACF of Marginal Standarized Residuals USA_Index"))

acf(garchmodel_std[["ARG_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals ARG_Price SGARCH T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("ARG_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals ARG_Price"))

acf(garchmodel_tstd[["EGPT_Price"]][["(2,2)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals EGPT_Price TGARCH T-Student (2,2)"))
acf(dccfith@mfit$stdresid[,match("EGPT_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals EGPT_Price"))

acf(garchmodel_skstd[["GHA_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals GHA_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("GHA_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals GHA_Price"))

acf(garchmodel_skstd[["KEN_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals KEN_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("KEN_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals KEN_Price"))

acf(garchmodel_estd[["MOR_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals MOR_Price EGARCH T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("MOR_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals MOR_Price"))

acf(garchmodel_skstd[["NIG_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals NIG_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("NIG_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals NIG_Price"))

acf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("SOUTHAFR_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals SOUTHAFR_Price"))

acf(garchmodel_estd[["TAN_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals TAN_Price EGARCH T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("TAN_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals TAN_Price"))

acf(garchmodel_std[["TUN_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals TUN_Price SGARCH T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("TUN_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals TUN_Price"))

par(mfrow = c(1,1))

#Comparación ACF para los residuos estandarizados al cuadrado univariantes ajustado 
#frente a los residuos estandarizados marginales al cuadrado del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75, cex.main = 1.2)
acf(garchmodel_eskstd[["USA_Index"]][["(2,2)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals USA_Index EGARCH Skewed T-Student (2,2)"))
acf(dccfith@mfit$stdresid[,match("USA_Index",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals USA_Index"))

acf(garchmodel_std[["ARG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals ARG_Price SGARCH T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("ARG_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals ARG_Price"))

acf(garchmodel_tstd[["EGPT_Price"]][["(2,2)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals EGPT_Price TGARCH T-Student (2,2)"))
acf(dccfith@mfit$stdresid[,match("EGPT_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals EGPT_Price"))

acf(garchmodel_skstd[["GHA_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals GHA_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("GHA_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals GHA_Price"))

acf(garchmodel_skstd[["KEN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals KEN_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("KEN_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals KEN_Price"))

acf(garchmodel_estd[["MOR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals MOR_Price EGARCH T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("MOR_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals MOR_Price"))

acf(garchmodel_skstd[["NIG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals NIG_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("NIG_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals NIG_Price"))

acf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("SOUTHAFR_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals SOUTHAFR_Price"))

acf(garchmodel_estd[["TAN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals TAN_Price EGARCH T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("TAN_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals TAN_Price"))

acf(garchmodel_std[["TUN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of SStandarized Squared Residuals TUN_Price SGARCH T-Student (1,1)"))
acf(dccfith@mfit$stdresid[,match("TUN_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals TUN_Price"))

par(mfrow = c(1,1))

#Comparación PACF para los residuos estandarizados univariantes ajustado frente a
#los residuos estandarizados marginales del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75, cex.main = 1.2)
pacf(garchmodel_eskstd[["USA_Index"]][["(2,2)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals USA_Index EGARCH Skewed T-Student (2,2)"))
pacf(dccfith@mfit$stdresid[,match("USA_Index",names)], 
     main = glue("PACF of Marginal Standarized Residuals USA_Index"))

pacf(garchmodel_std[["ARG_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals ARG_Price SGARCH T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("ARG_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals ARG_Price"))

pacf(garchmodel_tstd[["EGPT_Price"]][["(2,2)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals EGPT_Price TGARCH T-Student (2,2)"))
pacf(dccfith@mfit$stdresid[,match("EGPT_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals EGPT_Price"))

pacf(garchmodel_skstd[["GHA_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals GHA_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("GHA_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals GHA_Price"))

pacf(garchmodel_skstd[["KEN_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals KEN_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("KEN_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals KEN_Price"))

pacf(garchmodel_estd[["MOR_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals MOR_Price EGARCH T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("MOR_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals MOR_Price"))

pacf(garchmodel_skstd[["NIG_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals NIG_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("NIG_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals NIG_Price"))

pacf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("SOUTHAFR_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals SOUTHAFR_Price"))

pacf(garchmodel_estd[["TAN_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals TAN_Price EGARCH T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("TAN_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals TAN_Price"))

pacf(garchmodel_std[["TUN_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals TUN_Price SGARCH T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("TUN_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals TUN_Price"))

par(mfrow = c(1,1))

#Comparación PACF para los residuos estandarizados al cuadrado univariantes ajustado 
#frente a los residuos estandarizados marginales al cuadrado del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75, cex.main = 1.2)
pacf(garchmodel_eskstd[["USA_Index"]][["(2,2)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals USA_Index EGARCH Skewed T-Student (2,2)"))
pacf(dccfith@mfit$stdresid[,match("USA_Index",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals USA_Index"))

pacf(garchmodel_std[["ARG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals ARG_Price SGARCH T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("ARG_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals ARG_Price"))

pacf(garchmodel_tstd[["EGPT_Price"]][["(2,2)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals EGPT_Price TGARCH T-Student (2,2)"))
pacf(dccfith@mfit$stdresid[,match("EGPT_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals EGPT_Price"))

pacf(garchmodel_skstd[["GHA_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals GHA_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("GHA_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals GHA_Price"))

pacf(garchmodel_skstd[["KEN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals KEN_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("KEN_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals KEN_Price"))

pacf(garchmodel_estd[["MOR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals MOR_Price EGARCH T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("MOR_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals MOR_Price"))

pacf(garchmodel_skstd[["NIG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals NIG_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("NIG_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals NIG_Price"))

pacf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("SOUTHAFR_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals SOUTHAFR_Price"))

pacf(garchmodel_estd[["TAN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals TAN_Price EGARCH T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("TAN_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals TAN_Price"))

pacf(garchmodel_std[["TUN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals TUN_Price SGARCH T-Student (1,1)"))
pacf(dccfith@mfit$stdresid[,match("TUN_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals TUN_Price"))

par(mfrow = c(1,1))

#Multivariate Portmanteu-Test (Ljung-Box test for multivariate)
#H0: no hay autocorrelación
LjungBox(dccfith@mfit$stdresid,lags = (1:20),squared.residuals = FALSE)

#Multivariate Portmanteu-Test, squared.residuals = TRUE (ARCH EFFECTS)
#H0: no ARCH effect
LjungBox(dccfith@mfit$stdresid^2,lags = (1:20),squared.residuals = TRUE)

#CCF graph
#Estamos viendo si existe autocorrelación de los residuos de las series con USA
par(mfrow=c(3,1),mar=c(3,3,3,3), cex.main =1.5)
for (i in 2:10){
  ccf(dccfith@mfit$stdresid[,1],dccfitdefec@mfit$stdresid[,i],
      main = glue("Cross Correlation Standarized Residuals of {names[i]} with USA_Index"))
}

#---------------- DCC SIMÉTRICO T-STUDENT CON SELECCIÓN UNIV DIAGONAL --------

#Recordamos que los modelos son:
resumenselectdiagindv

#DCC SIMÉTRICO T-STUDENT
specdiagusa <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,2)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="std")

specdiagarg <- ugarchspec(variance.model=list(model="fGARCH", submodel = "TGARCH"
                                              ,garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="std")

specdiagegpt <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(4,3)),
                           mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                           distribution.model="std")

specdiaggha <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(2,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="std")

specdiagken <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="std")

specdiagmor <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="norm")

specdiagnig <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="std")

specdiagsouthafr <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                               distribution.model="sstd")

specdiagtan <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="std")

specdiagtun <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="std")

multispec <- multispec(c(specdiagusa,specdiagarg,specdiagegpt,specdiaggha,
                         specdiagken,specdiagmor,specdiagnig,specdiagsouthafr,
                         specdiagtan,specdiagtun))

multift <- multifit(multispec,rend_zoo)

#Modelo básico: DCC GARCH T-STUDENT (1,1)
dccspecdiag <- dccspec(uspec = multispec, dccOrder= c(1,1), model = c("DCC"), 
                       distribution = "mvt")
dccfitdiag <- dccfit(dccspecdiag, data = rend_zoo, 
                     fit.control =  list(eval.se = TRUE),fit = multift,
                     solver="solnp", solver.control = list(tol = 1e-12))
dccfitdiag

rcor(dccfitdiag, type="R")#[,,'2005-10-17'] #array of corr
rcov(dccfitdiag)


#DIAGNOSIS

#Gráficos generales
#¿Son definidas positivas?
for (i in seq_along(time)){
  print(isPositiveDefinite(rcov(dccfitdiag)[,,i]))
}

#Marginal Standarized Residuals
par(mfrow = c(4,1), mar =c(2,2,2,2))
for (i in seq_along(names)){
  plot(as.timeSeries(dccfitdiag@mfit$stdresid[,i]),lwd = 1.8,
       main = glue("Standarized Residuals {names[i]}"))
}
par(mfrow = c(1,1))

#Square Residuals
par(mfrow = c(4,1), mar =c(2,2,2,2))
for (i in seq_along(names)){
  plot(as.timeSeries(dccfitdiag@mfit$stdresid[,i]^2), lwd = 1.8,
       main = glue("Square Standarized Residuals {names[i]}"))
}
par(mfrow = c(1,1))

#Coeficientes
dccfitdiag

#Estimated volatility vs realized volatility (abs returns)
par(mar=c(2,2,4,2))
plot(dccfitdiag, which=2, series=c(1,2))
plot(dccfitdiag, which=2, series=c(3,4))
plot(dccfitdiag, which=2, series=c(5,6))
plot(dccfitdiag, which=2, series=c(7,8))
plot(dccfitdiag, which=2, series=c(9,10))

#Time varianting correlation among series with USA
par(mfrow = c(3,1), mar =c(3,3,3,3), cex.main=1.5)
for (i in names[2:10]){
  plot(as.timeSeries(rcor(dccfitdiag, type="R")["USA_Index",i,]), lwd = 1.8,
       main = glue("Correlación condicional USA_Index y {i}"))
}
par(mfrow = c(1,1))

#Time varianting covariance among series with USA
par(mfrow = c(3,1), mar =c(3,3,3,3))
for (i in seq_along(names)){
  plot(as.timeSeries(rcov(dccfitdiag)["USA_Index",i,], type = "l"), lwd = 1.8,
       main = glue("Covarianza condicional {names[i]}"))
}
par(mfrow = c(1,1))

#Comparación ACF para los residuos estandarizados univariantes ajustado frente a
#los residuos estandarizados marginales del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75,cex.main=1.4)
acf(garchmodel_std[["USA_Index"]][["(2,2)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals USA_Index SGARCH T-Student (2,2)"))
acf(dccfitdiag@mfit$stdresid[,match("USA_Index",names)], 
    main = glue("ACF of Marginal Standarized Residuals USA_Index"))

acf(garchmodel_tstd[["ARG_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals ARG_Price TGARCH T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("ARG_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals ARG_Price"))

acf(garchmodel_estd[["EGPT_Price"]][["(4,3)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals EGPT_Price EGARCH T-Student (4,3)"))
acf(dccfitdiag@mfit$stdresid[,match("EGPT_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals EGPT_Price"))

acf(garchmodel_estd[["GHA_Price"]][["(2,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals GHA_Price EGARCH T-Student (2,1)"))
acf(dccfitdiag@mfit$stdresid[,match("GHA_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals GHA_Price"))

acf(garchmodel_std[["KEN_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals KEN_Price SGARCH T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("KEN_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals KEN_Price"))

acf(garchmodel_norm[["MOR_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals MOR_Price SGARCH Normal (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("MOR_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals MOR_Price"))

acf(garchmodel_estd[["NIG_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals NIG_Price EGARCH T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("NIG_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals NIG_Price"))

acf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("SOUTHAFR_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals SOUTHAFR_Price"))

acf(garchmodel_estd[["TAN_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals TAN_Price EGARCH T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("TAN_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals TAN_Price"))

acf(garchmodel_std[["TUN_Price"]][["(1,1)"]]@fit[["z"]], 
    main = glue("ACF of Standarized Residuals TUN_Price SGARCH T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("TUN_Price",names)], 
    main = glue("ACF of Marginal Standarized Residuals TUN_Price"))

par(mfrow = c(1,1))

#Comparación ACF para los residuos estandarizados al cuadrado univariantes ajustado 
#frente a los residuos estandarizados marginales al cuadrado del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75,cex.main=1.4)
acf(garchmodel_std[["USA_Index"]][["(2,2)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals USA_Index SGARCH T-Student (2,2)"))
acf(dccfitdiag@mfit$stdresid[,match("USA_Index",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals USA_Index"))

acf(garchmodel_tstd[["ARG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals ARG_Price TGARCH T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("ARG_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals ARG_Price"))

acf(garchmodel_estd[["EGPT_Price"]][["(4,3)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals EGPT_Price EGARCH T-Student (4,3)"))
acf(dccfitdiag@mfit$stdresid[,match("EGPT_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals EGPT_Price"))

acf(garchmodel_estd[["GHA_Price"]][["(2,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals GHA_Price EGARCH T-Student (2,1)"))
acf(dccfitdiag@mfit$stdresid[,match("GHA_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals GHA_Price"))

acf(garchmodel_std[["KEN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of SStandarized Squared Residuals KEN_Price SGARCH T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("KEN_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals KEN_Price"))

acf(garchmodel_norm[["MOR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals MOR_Price SGARCH Normal (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("MOR_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals MOR_Price"))

acf(garchmodel_estd[["NIG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals NIG_Price EGARCH T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("NIG_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals NIG_Price"))

acf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("SOUTHAFR_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals SOUTHAFR_Price"))

acf(garchmodel_estd[["TAN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals TAN_Price EGARCH T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("TAN_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals TAN_Price"))

acf(garchmodel_std[["TUN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
    main = glue("ACF of Standarized Squared Residuals TUN_Price SGARCH T-Student (1,1)"))
acf(dccfitdiag@mfit$stdresid[,match("TUN_Price",names)]^2, 
    main = glue("ACF of Marginal Standarized Squared Residuals TUN_Price"))

par(mfrow = c(1,1))

#Comparación PACF para los residuos estandarizados univariantes ajustado frente a
#los residuos estandarizados marginales del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75,cex.main=1.4)
pacf(garchmodel_eskstd[["USA_Index"]][["(2,2)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals USA_Index EGARCH Skewed T-Student (2,2)"))
pacf(dccfitdiag@mfit$stdresid[,match("USA_Index",names)], 
     main = glue("PACF of Marginal Standarized Residuals USA_Index"))

pacf(garchmodel_std[["ARG_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals ARG_Price SGARCH T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("ARG_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals ARG_Price"))

pacf(garchmodel_tstd[["EGPT_Price"]][["(2,2)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals EGPT_Price TGARCH T-Student (2,2)"))
pacf(dccfitdiag@mfit$stdresid[,match("EGPT_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals EGPT_Price"))

pacf(garchmodel_skstd[["GHA_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals GHA_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("GHA_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals GHA_Price"))

pacf(garchmodel_skstd[["KEN_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals KEN_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("KEN_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals KEN_Price"))

pacf(garchmodel_estd[["MOR_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals MOR_Price EGARCH T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("MOR_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals MOR_Price"))

pacf(garchmodel_skstd[["NIG_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals NIG_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("NIG_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals NIG_Price"))

pacf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("SOUTHAFR_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals SOUTHAFR_Price"))

pacf(garchmodel_estd[["TAN_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals TAN_Price EGARCH T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("TAN_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals TAN_Price"))

pacf(garchmodel_std[["TUN_Price"]][["(1,1)"]]@fit[["z"]], 
     main = glue("PACF of Standarized Residuals TUN_Price SGARCH T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("TUN_Price",names)], 
     main = glue("PACF of Marginal Standarized Residuals TUN_Price"))

par(mfrow = c(1,1))

#Comparación PACF para los residuos estandarizados al cuadrado univariantes ajustado 
#frente a los residuos estandarizados marginales al cuadrado del modelo multivariante
par(mfrow = c(2,2), mar =c(4,4,4,4), cex=0.75,cex.main=1.4)
pacf(garchmodel_eskstd[["USA_Index"]][["(2,2)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals USA_Index EGARCH Skewed T-Student (2,2)"))
pacf(dccfitdiag@mfit$stdresid[,match("USA_Index",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals USA_Index"))

pacf(garchmodel_std[["ARG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals ARG_Price SGARCH T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("ARG_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals ARG_Price"))

pacf(garchmodel_tstd[["EGPT_Price"]][["(2,2)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals EGPT_Price TGARCH T-Student (2,2)"))
pacf(dccfitdiag@mfit$stdresid[,match("EGPT_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals EGPT_Price"))

pacf(garchmodel_skstd[["GHA_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals GHA_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("GHA_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals GHA_Price"))

pacf(garchmodel_skstd[["KEN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals KEN_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("KEN_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals KEN_Price"))

pacf(garchmodel_estd[["MOR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals MOR_Price EGARCH T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("MOR_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals MOR_Price"))

pacf(garchmodel_skstd[["NIG_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals NIG_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("NIG_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals NIG_Price"))

pacf(garchmodel_skstd[["SOUTHAFR_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals SOUTHAFR_Price SGARCH Skewed T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("SOUTHAFR_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals SOUTHAFR_Price"))

pacf(garchmodel_estd[["TAN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals TAN_Price EGARCH T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("TAN_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals TAN_Price"))

pacf(garchmodel_std[["TUN_Price"]][["(1,1)"]]@fit[["z"]]^2, 
     main = glue("PACF of Standarized Squared Residuals TUN_Price SGARCH T-Student (1,1)"))
pacf(dccfitdiag@mfit$stdresid[,match("TUN_Price",names)]^2, 
     main = glue("PACF of Marginal Standarized Squared Residuals TUN_Price"))

par(mfrow = c(1,1))

#Multivariate Portmanteu-Test (Ljung-Box test for multivariate)
#H0: no hay autocorrelación
LjungBox(dccfitdiag@mfit$stdresid,lags = (1:20),squared.residuals = FALSE)

#Multivariate Portmanteu-Test, squared.residuals = TRUE (ARCH EFFECTS)
#H0: no ARCH effect
LjungBox(dccfitdiag@mfit$stdresid^2,lags = (1:20),squared.residuals = TRUE)

#CCF graph
#Estamos viendo si existe autocorrelación de los residuos de las series con USA
par(mfrow=c(3,1),mar=c(3,3,3,3), cex.main=1.5)
for (i in 2:10){
  ccf(dccfitdiag@mfit$stdresid[,1],dccfitdefec@mfit$stdresid[,i],
      main = glue("Cross Correlation Standarized Residuals of {names[i]} with USA_Index"))
}

######################### CONCLUSIONES MODELO DCC ##############################

pp <- list()
for (i in names[2:10]){
  cordf <- data.frame()
  cordf <- cbind(as.data.frame(rownames(as.timeSeries(rcor(dccfitdefec, type="R")["USA_Index",i,]))),
                 as.data.frame(as.timeSeries(rcor(dccfitdefec, type="R")["USA_Index",i,])))
  cordf <- cbind.data.frame(cordf,as.timeSeries(rcor(dccfitv, type="R")["USA_Index",i,]))
  cordf <- cbind.data.frame(cordf,as.timeSeries(rcor(dccfith, type="R")["USA_Index",i,]))
  cordf <- cbind.data.frame(cordf,as.timeSeries(rcor(dccfitdiag, type="R")["USA_Index",i,]))
  colnames(cordf) <- c("Date","DCC Básico","DCC Vertical","DCC Horizontal","DCC Diagonal")
  cordf <- melt(cordf, id ="Date")
  colnames(cordf)[2] = "DCC Model"
  pp[[i]] <- ggplot(cordf, aes(y = value, x = as.Date(Date), color = `DCC Model`)) + geom_line() +
    xlab("Date") + ylab("Correlation") + ggtitle(glue("{i} - USA_Index")) +
    guides(fill=guide_legend(title="DCC Model")) + ylim(min(cordf$value),max(cordf$value)) +
    theme(plot.title = element_text(size = 10))
  
}
p_no_legend <- lapply(pp, function(x) x + theme(legend.position = "none"))
legend <- cowplot::get_legend(pp[[1]] + theme(legend.position = "bottom"))
title <- cowplot::ggdraw()
p_grid <- cowplot::plot_grid(plotlist = p_no_legend, ncol = 3)
cowplot::plot_grid(title,p_grid,legend,ncol = 1, rel_heights = c(0.4, 10, 0.6))
