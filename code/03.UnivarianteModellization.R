#rm(list = ls())
#setwd()

####################################### LIBRARIES #############################
library(parallel)
library(doParallel)
library(rugarch)
library(glue)
library(zoo)

######################### MODELIZACIÓN UNIVARIANTE ##############################
load("rend_df.rda")

rend_zoo <- read.zoo(rend_df)
head(rend_zoo)
save(rend_zoo,file="rend_zoo.rda")

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

#------------------------------------------------------------------------------#
###SGARCH: SYMETRIC Generalized Autoregressive Conditional Heterocedasticity####
#------------------------------------------------------------------------------#

#SGARCH, Normal distribution
maxorder <- 5 
allorders_norm  <- list()
garchmodel_norm <- list()
listnames <- data.frame()
names <- colnames(rend_zoo)

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="norm")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_norm[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("SGARCH Norm {i} ({archp},{garchq})")
      allorders_norm[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  if(length(allorders_norm[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_norm[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_norm[[i]]) <- dput(listnames[1:length(allorders_norm[[i]][,1]),1])
  }
  scounter <- scounter+1
}


#SGARCH, Skewed Normal distribution
maxorder <- 5 
allorders_sknorm  <- list()
garchmodel_sknorm <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="snorm")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_sknorm[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("SGARCH Skewed Norm {i} ({archp},{garchq})")
      allorders_sknorm[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  if(length(allorders_sknorm[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_sknorm[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_sknorm[[i]]) <- dput(listnames[1:length(allorders_sknorm[[i]][,1]),1])
  }
  scounter <- scounter+1
  
}


#SGARCH, T Student distribution
maxorder <- 5 
allorders_std  <- list()
garchmodel_std <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="std")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_std[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("SGARCH T-Student {i} ({archp},{garchq})")
      allorders_std[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  if(length(allorders_std[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_std[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_std[[i]]) <- dput(listnames[1:length(allorders_std[[i]][,1]),1])
  }
  scounter <- scounter+1
}


#SGARCH, Skew T Student distribution
maxorder <- 5 
allorders_skstd  <- list()
garchmodel_skstd <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="sstd")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_skstd[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("SGARCH Skewed T-Student {i} ({archp},{garchq})")
      allorders_skstd[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  if(length(allorders_skstd[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_skstd[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_skstd[[i]]) <- dput(listnames[1:length(allorders_skstd[[i]][,1]),1])
  }
  scounter <- scounter+1
}

#-------------------------------------------------------------------------------#
##APARCH: ASSIMETRIC POWER Autoregressive Conditional Heterocedasticity######
#------------------------------------------------------------------------------#

#APARCH, Normal Distribution
maxorder <- 5 
allorders_apnorm  <- list()
garchmodel_apnorm <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="norm")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_apnorm[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("APARCH Norm {i} ({archp},{garchq})")
      allorders_apnorm[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_apnorm[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_apnorm[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_apnorm[[i]]) <- dput(listnames[1:length(allorders_apnorm[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}


#APARCH, Skewed Normal distribution
maxorder <- 5 
allorders_apsknorm  <- list()
garchmodel_apsknorm <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="snorm")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_apsknorm[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("APARCH Skewed Norm {i} ({archp},{garchq})")
      allorders_apsknorm[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  if(length(allorders_apsknorm[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_apsknorm[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_apsknorm[[i]]) <- dput(listnames[1:length(allorders_apsknorm[[i]][,1]),1])
  }
  scounter <- scounter+1
}


#APARCH, T Student distribution
maxorder <- 5 
allorders_apstd  <- list()
garchmodel_apstd <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="std")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_apstd[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("APARCH T-Student {i} ({archp},{garchq})")
      allorders_apstd[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_apstd[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_apstd[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_apstd[[i]]) <- dput(listnames[1:length(allorders_apstd[[i]][,1]),1])
  }
  scounter <- scounter+1
}


#APARCH, Skew T Student distribution
maxorder <- 5 
allorders_apskstd  <- list()
garchmodel_apskstd <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="sstd")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_apskstd[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("APARCH Skewed T-Student {i} ({archp},{garchq})")
      allorders_apskstd[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_apskstd[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_apskstd[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_apskstd[[i]]) <- dput(listnames[1:length(allorders_apskstd[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}


#-------------------------------------------------------------------------------#
##TGARCH: TRESHOLD Generalized Autoregressive Conditional Heterocedasticity####
#------------------------------------------------------------------------------#

#TGARCH, Normal Distribution
maxorder <- 5 
allorders_tnorm  <- list()
garchmodel_tnorm <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="fGARCH", submodel = "TGARCH",
                                                garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="norm")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_tnorm[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("TGARCH Norm {i} ({archp},{garchq})")
      allorders_tnorm[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_tnorm[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_tnorm[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_tnorm[[i]]) <- dput(listnames[1:length(allorders_tnorm[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}


#TGARCH, Skewed Normal distribution
maxorder <- 5 
allorders_tsknorm  <- list()
garchmodel_tsknorm <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="fGARCH", submodel = "TGARCH",
                                                garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="snorm")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_tsknorm[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("TGARCH Skewed Norm {i} ({archp},{garchq})")
      allorders_tsknorm[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_tsknorm[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_tsknorm[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_tsknorm[[i]]) <- dput(listnames[1:length(allorders_tsknorm[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}


#TGARCH, T Student distribution
maxorder <- 5 
allorders_tstd  <- list()
garchmodel_tstd <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="fGARCH", submodel = "TGARCH",
                                                garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="std")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_tstd[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("TGARCH T-Student {i} ({archp},{garchq})")
      allorders_tstd[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_tstd[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_tstd[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_tstd[[i]]) <- dput(listnames[1:length(allorders_tstd[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}


#TGARCH, Skew T Student distribution
maxorder <- 5 
allorders_tskstd  <- list()
garchmodel_tskstd <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="fGARCH", submodel = "TGARCH",
                                                garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="sstd")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_tskstd[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("TGARCH Skewed T-Student {i} ({archp},{garchq})")
      allorders_tskstd[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_tskstd[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_tskstd[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_tskstd[[i]]) <- dput(listnames[1:length(allorders_tskstd[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}


#-------------------------------------------------------------------------------#
###EGARCH: EXPONENTIAL Generalized Autoregressive Conditional Heterocedasticity####
#------------------------------------------------------------------------------#

#EGARCH, Normal Distribution
maxorder <- 5 
allorders_enorm  <- list()
garchmodel_enorm <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="norm")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_enorm[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("EGARCH Norm {i} ({archp},{garchq})")
      allorders_enorm[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_enorm[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_enorm[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_enorm[[i]]) <- dput(listnames[1:length(allorders_enorm[[i]][,1]),1])
  }
  scounter <- scounter+1
}


#EGARCH, Skewed Normal distribution
maxorder <- 5 
allorders_esknorm  <- list()
garchmodel_esknorm <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="snorm")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_esknorm[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("EGARCH Skewed Norm {i} ({archp},{garchq})")
      allorders_esknorm[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_esknorm[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_esknorm[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_esknorm[[i]]) <- dput(listnames[1:length(allorders_esknorm[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}


#EGARCH, T Student distribution
maxorder <- 5 
allorders_estd  <- list()
garchmodel_estd <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      if(i == "EGPT_Price" && archp==5 && garchq==5) next #este modelo específico da problemas
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="std")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_estd[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("EGARCH T-Student {i} ({archp},{garchq})")
      allorders_estd[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_estd[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_estd[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_estd[[i]]) <- dput(listnames[1:length(allorders_estd[[i]][,1]),1])
  }
  scounter <- scounter+1
}


#EGARCH, Skew T Student distribution
maxorder <- 5 
allorders_eskstd  <- list()
garchmodel_eskstd <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="sstd")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_eskstd[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("EGARCH Skewed T-Student {i} ({archp},{garchq})")
      allorders_eskstd[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  if(length(allorders_eskstd[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_eskstd[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_eskstd[[i]]) <- dput(listnames[1:length(allorders_eskstd[[i]][,1]),1])
  }
  scounter <- scounter+1
}


#-------------------------------------------------------------------------------#
###NAGARCH: NONLINEAR ASSYMETRIC Generalized Autoregressive Conditional Heterocedasticity####
#------------------------------------------------------------------------------#

#NAGARCH, Normal Distribution
maxorder <- 5 
allorders_nanorm  <- list()
garchmodel_nanorm <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="fGARCH",submodel = "NAGARCH",
                                                garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="norm")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_nanorm[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("NAGARCH Norm {i} ({archp},{garchq})")
      allorders_nanorm[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_nanorm[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_nanorm[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_nanorm[[i]]) <- dput(listnames[1:length(allorders_nanorm[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}


#NAGARCH, Skewed Normal distribution
maxorder <- 5 
allorders_nasknorm  <- list()
garchmodel_nasknorm <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="fGARCH",submodel = "NAGARCH",
                                                garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="snorm")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_nasknorm[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("NAGARCH Skewed Norm {i} ({archp},{garchq})")
      allorders_nasknorm[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_nasknorm[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_nasknorm[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_nasknorm[[i]]) <- dput(listnames[1:length(allorders_nasknorm[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}


#NAGARCH, T Student distribution
maxorder <- 5 
allorders_nastd  <- list()
garchmodel_nastd <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      if(i == "EGPT_Price" && archp==2 && garchq==1) next #este modelo da problemas
      if(i == "GHA_Price" && archp==2 && garchq==1) next #este modelo da problemas
      if(i == "GHA_Price" && archp==2 && garchq==2) next #este modelo da problemas
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="fGARCH",submodel = "NAGARCH",
                                                garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="std")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_nastd[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("NAGARCH T-Student {i} ({archp},{garchq})")
      allorders_nastd[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_nastd[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_nastd[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_nastd[[i]]) <- dput(listnames[1:length(allorders_nastd[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}


#NAGARCH, Skew T Student distribution
maxorder <- 5 
allorders_naskstd  <- list()
garchmodel_naskstd <- list()
listnames <- data.frame()

scounter <- 1 # Contador de acciones
for (i in names){
  cat(paste0(i,"\n"))
  temp_rugarch <- data.frame()  # Matriz de almacenaje
  mcounter <- 1
  for (archp in 0:maxorder){
    for (garchq in 0:maxorder){
      if(archp==0 && garchq==0) next    # GARCH(0,0) proceso degenerado
      if(i == "ARG_Price" && archp==4 && garchq==3) next #este modelo da problemas
      if(i == "KEN_Price" && archp==3 && garchq==0) next #este modelo da problemas
      if(i == "NIG_Price" && archp==2 && garchq==4) next
      if(i == "NIG_Price" && archp==3 && garchq==2) next
      cat(paste0(archp, "-", garchq, "\n"))  # Imprime en pantalla los órdenes seleccionados
      mod.spec = ugarchspec(variance.model=list(model="fGARCH",submodel = "NAGARCH",
                                                garchOrder=c(archp,garchq)),
                            mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                            distribution.model="sstd")  # Se puede cambiar esta especificación
      garchmodel <- try(ugarchfit(mod.spec, data= rend_zoo[,i], solver="hybrid",
                                  solver.control = list(tol = 1e-12)), silent=TRUE)
      garchmodel_naskstd[[i]][[glue("({archp},{garchq})")]] <- garchmodel
      
      if('try-error' %in% class(garchmodel)) next
      #        if(garchmodel@fit$convergence==1) next
      if(garchmodel@fit$convergence!=0) next
      
      temp_rugarch[mcounter,1]<- archp
      temp_rugarch[mcounter,2]<- garchq
      temp_rugarch[mcounter,3]<- infocriteria(garchmodel)[1]  # AKaike AIC
      temp_rugarch[mcounter,4]<- infocriteria(garchmodel)[2]  # Bayesian Information Criteria BIC
      temp_rugarch[mcounter,5]<- infocriteria(garchmodel)[3]  # Shibata Information Criteria
      temp_rugarch[mcounter,6]<- infocriteria(garchmodel)[4]  # Hannan-Quinn Information Criteria
      
      names(temp_rugarch) <- c("ARCH", "GARCH", "AIC","BIC","Shibata","Hannan-Quinn")
      listnames[mcounter,1] <- glue("NAGARCH Skewed T-Student {i} ({archp},{garchq})")
      allorders_naskstd[[i]] <- temp_rugarch
      
      cat(infocriteria(garchmodel)[1:4],"\n")  # Imprime el resultado por pantalla
      mcounter <- mcounter+1
    }
  }
  
  if(length(allorders_naskstd[[i]][,1]) == length(listnames[,1])){
    rownames(allorders_naskstd[[i]]) <- dput(listnames[,1])
  } else{
    rownames(allorders_naskstd[[i]]) <- dput(listnames[1:length(allorders_naskstd[[i]][,1]),1])
  }
  
  scounter <- scounter+1
}

stopCluster(cluster)
registerDoSEQ()
