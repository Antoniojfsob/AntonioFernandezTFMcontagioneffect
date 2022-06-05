#rm(list = ls())
#setwd()
#################################### LIBRERÍAS #################################

####################### CONTRASTE DE HIPÓTESIS (RESULTADOS)#####################
load("rend_zoo.rda")
#load data from MultivariateModellization.R
names <- colnames(rend_zoo)

#Fechas: "2007-07-17","2007-08-09","2007-09-03","2007-10-01","2007-10-30","2007-12-06"
listcontraste <- list()

########################## MEQMA ###########################################

#Eliminamos las 10 primeras observaciones por problemas de distorsion
cor_meqma_all10 <- cor_meqma_all[,,-c(1:10)]

#Contraste de hipótesis
#2007-07-17
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:651)){#661-10 = 651
    precrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:651])
listcontraste[["MEQMA"]][["2007-07-17"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(652:1293)){#quitando 10
    postcrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[652:1293])
listcontraste[["MEQMA"]][["2007-07-17"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEQMA"]][["2007-07-17"]][["contraste"]][[i]] <- t.test(listcontraste[["MEQMA"]][["2007-07-17"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEQMA"]][["2007-07-17"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEQMA"]][["2007-07-17"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-08-09
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:668)){
    precrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:668])
listcontraste[["MEQMA"]][["2007-08-09"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(669:1293)){
    postcrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[669:1293])
listcontraste[["MEQMA"]][["2007-08-09"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEQMA"]][["2007-08-09"]][["contraste"]][[i]] <- t.test(listcontraste[["MEQMA"]][["2007-08-09"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEQMA"]][["2007-08-09"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEQMA"]][["2007-08-09"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-09-03
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:685)){
    precrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:685])
listcontraste[["MEQMA"]][["2007-09-03"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(686:1293)){
    postcrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[686:1293])
listcontraste[["MEQMA"]][["2007-09-03"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEQMA"]][["2007-09-03"]][["contraste"]][[i]] <- t.test(listcontraste[["MEQMA"]][["2007-09-03"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEQMA"]][["2007-09-03"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEQMA"]][["2007-09-03"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-10-01
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:705)){
    precrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:705])
listcontraste[["MEQMA"]][["2007-10-01"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(706:1293)){
    postcrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[706:1293])
listcontraste[["MEQMA"]][["2007-10-01"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEQMA"]][["2007-10-01"]][["contraste"]][[i]] <- t.test(listcontraste[["MEQMA"]][["2007-10-01"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEQMA"]][["2007-10-01"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEQMA"]][["2007-10-01"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-10-30
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:726)){
    precrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:726])
listcontraste[["MEQMA"]][["2007-10-30"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(727:1293)){
    postcrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[727:1293])
listcontraste[["MEQMA"]][["2007-10-30"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEQMA"]][["2007-10-30"]][["contraste"]][[i]] <- t.test(listcontraste[["MEQMA"]][["2007-10-30"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEQMA"]][["2007-10-30"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEQMA"]][["2007-10-30"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-12-06
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:753)){
    precrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:753])
listcontraste[["MEQMA"]][["2007-12-06"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(754:1293)){
    postcrisis[icounter,xcounter] <-cor_meqma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[754:1293])
listcontraste[["MEQMA"]][["2007-12-06"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEQMA"]][["2007-12-06"]][["contraste"]][[i]] <- t.test(listcontraste[["MEQMA"]][["2007-12-06"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEQMA"]][["2007-12-06"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEQMA"]][["2007-12-06"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


############################### MEWMA ########################################

#Eliminamos las 10 primeras observaciones por problemas de distorsion
cor_mewma_all10 <- cor_mewma_all[,,-c(1:10)]

tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
#2007-07-17
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:651)){#661-10 = 651
    precrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:651])
listcontraste[["MEWMA"]][["2007-07-17"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(652:1293)){#quitando 10
    postcrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[652:1293])
listcontraste[["MEWMA"]][["2007-07-17"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEWMA"]][["2007-07-17"]][["contraste"]][[i]] <- t.test(listcontraste[["MEWMA"]][["2007-07-17"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEWMA"]][["2007-07-17"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEWMA"]][["2007-07-17"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-08-09
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:668)){
    precrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:668])
listcontraste[["MEWMA"]][["2007-08-09"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(669:1293)){
    postcrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[669:1293])
listcontraste[["MEWMA"]][["2007-08-09"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEWMA"]][["2007-08-09"]][["contraste"]][[i]] <- t.test(listcontraste[["MEWMA"]][["2007-08-09"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEWMA"]][["2007-08-09"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEWMA"]][["2007-08-09"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-09-03
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:685)){
    precrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:685])
listcontraste[["MEWMA"]][["2007-09-03"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(686:1293)){
    postcrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[686:1293])
listcontraste[["MEWMA"]][["2007-09-03"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEWMA"]][["2007-09-03"]][["contraste"]][[i]] <- t.test(listcontraste[["MEWMA"]][["2007-09-03"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEWMA"]][["2007-09-03"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEWMA"]][["2007-09-03"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio



#2007-10-01
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:705)){
    precrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:705])
listcontraste[["MEWMA"]][["2007-10-01"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(706:1293)){
    postcrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[706:1293])
listcontraste[["MEWMA"]][["2007-10-01"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEWMA"]][["2007-10-01"]][["contraste"]][[i]] <- t.test(listcontraste[["MEWMA"]][["2007-10-01"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEWMA"]][["2007-10-01"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEWMA"]][["2007-10-01"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-10-30
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:726)){
    precrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:726])
listcontraste[["MEWMA"]][["2007-10-30"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(727:1293)){
    postcrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[727:1293])
listcontraste[["MEWMA"]][["2007-10-30"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEWMA"]][["2007-10-30"]][["contraste"]][[i]] <- t.test(listcontraste[["MEWMA"]][["2007-10-30"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEWMA"]][["2007-10-30"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEWMA"]][["2007-10-30"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio
#Conclusiones: todos tienen contagio


#2007-12-06
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
tt <- tt[-c(1:10)]
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:753)){
    precrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:753])
listcontraste[["MEWMA"]][["2007-12-06"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(754:1293)){
    postcrisis[icounter,xcounter] <-cor_mewma_all10[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[754:1293])
listcontraste[["MEWMA"]][["2007-12-06"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["MEWMA"]][["2007-12-06"]][["contraste"]][[i]] <- t.test(listcontraste[["MEWMA"]][["2007-12-06"]][["precrisis"]][[i]],
                                                                         listcontraste[["MEWMA"]][["2007-12-06"]][["postcrisis"]][[i]],
                                                                         var.equal = FALSE)
  print(listcontraste[["MEWMA"]][["2007-12-06"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


## Selección GARCH Normal (1,1) Univariante: DCC GARCH NORMAL (1,1) #########

#2007-07-17
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:661)){
    precrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:661])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-07-17"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(662:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[662:1303])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-07-17"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-07-17"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-07-17"]][["precrisis"]][[i]],
                                                                                                  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-07-17"]][["postcrisis"]][[i]],
                                                                                                  var.equal = FALSE)
  print(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-07-17"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-08-09
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:678)){
    precrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:678])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-08-09"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(679:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[679:1303])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-08-09"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-08-09"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-08-09"]][["precrisis"]][[i]],
                                                                                                  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-08-09"]][["postcrisis"]][[i]],
                                                                                                  var.equal = FALSE)
  print(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-08-09"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-09-03
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:695)){
    precrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:695])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-09-03"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(696:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[696:1303])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-09-03"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-09-03"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-09-03"]][["precrisis"]][[i]],
                                                                                                  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-09-03"]][["postcrisis"]][[i]],
                                                                                                  var.equal = FALSE)
  print(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-09-03"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio



#2007-10-01
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:715)){
    precrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:715])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-01"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(716:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[716:1303])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-01"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-01"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-01"]][["precrisis"]][[i]],
                                                                                                  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-01"]][["postcrisis"]][[i]],
                                                                                                  var.equal = FALSE)
  print(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-01"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-10-30
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:736)){
    precrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:736])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-30"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(737:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[737:1303])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-30"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-30"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-30"]][["precrisis"]][[i]],
                                                                                                  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-30"]][["postcrisis"]][[i]],
                                                                                                  var.equal = FALSE)
  print(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-10-30"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-12-06
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:763)){
    precrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:763])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-12-06"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(764:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdefec, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[764:1303])
listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-12-06"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-12-06"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-12-06"]][["precrisis"]][[i]],
                                                                                                  listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-12-06"]][["postcrisis"]][[i]],
                                                                                                  var.equal = FALSE)
  print(listcontraste[["SELEC DEFECTO DCC NORMAL (1,1)"]][["2007-12-06"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio

### Selección VERTICAL Univariante: DCC GARCH T-Student (1,1) #################

#2007-07-17
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:661)){
    precrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:661])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-07-17"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(662:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[662:1303])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-07-17"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-07-17"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-07-17"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-07-17"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-07-17"]][["contraste"]][[i]])
}

#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio

#2007-08-09
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:678)){
    precrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:678])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-08-09"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(679:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[679:1303])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-08-09"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-08-09"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-08-09"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-08-09"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-08-09"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-09-03
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:695)){
    precrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:695])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-09-03"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(696:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[696:1303])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-09-03"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-09-03"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-09-03"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-09-03"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-09-03"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio



#2007-10-01
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:715)){
    precrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:715])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-01"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(716:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[716:1303])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-01"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-01"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-01"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-01"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-01"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-10-30
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:736)){
    precrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:736])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-30"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(737:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[737:1303])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-30"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-30"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-30"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-30"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-10-30"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-12-06
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:763)){
    precrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:763])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-12-06"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(764:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitv, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[764:1303])
listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-12-06"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-12-06"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-12-06"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-12-06"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC VERTICAL DCC T-Student (1,1)"]][["2007-12-06"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


##Selección HORIZONTAL Univariante: DCC GARCH T-Student (1,1) #################

#2007-07-17
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:661)){
    precrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:661])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-07-17"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(662:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[662:1303])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-07-17"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-07-17"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-07-17"]][["precrisis"]][[i]],
                                                                                                        listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-07-17"]][["postcrisis"]][[i]],
                                                                                                        var.equal = FALSE)
  print(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-07-17"]][["contraste"]][[i]])
}

#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-08-09
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:678)){
    precrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:678])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-08-09"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(679:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[679:1303])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-08-09"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-08-09"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-08-09"]][["precrisis"]][[i]],
                                                                                                        listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-08-09"]][["postcrisis"]][[i]],
                                                                                                        var.equal = FALSE)
  print(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-08-09"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-09-03
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:695)){
    precrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:695])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-09-03"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(696:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[696:1303])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-09-03"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-09-03"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-09-03"]][["precrisis"]][[i]],
                                                                                                        listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-09-03"]][["postcrisis"]][[i]],
                                                                                                        var.equal = FALSE)
  print(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-09-03"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio



#2007-10-01
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:715)){
    precrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:715])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-01"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(716:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[716:1303])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-01"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-01"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-01"]][["precrisis"]][[i]],
                                                                                                        listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-01"]][["postcrisis"]][[i]],
                                                                                                        var.equal = FALSE)
  print(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-01"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-10-30
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:736)){
    precrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:736])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-30"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(737:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[737:1303])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-30"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-30"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-30"]][["precrisis"]][[i]],
                                                                                                        listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-30"]][["postcrisis"]][[i]],
                                                                                                        var.equal = FALSE)
  print(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-10-30"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-12-06
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:763)){
    precrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:763])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-12-06"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(764:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfith, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[764:1303])
listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-12-06"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-12-06"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-12-06"]][["precrisis"]][[i]],
                                                                                                        listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-12-06"]][["postcrisis"]][[i]],
                                                                                                        var.equal = FALSE)
  print(listcontraste[["SELEC HORIZONTAL DCC T-Student (1,1)"]][["2007-12-06"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


##Selección DIAGONAL Univariante: DCC GARCH T-Student (1,1) ####################

#2007-07-17
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:661)){
    precrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:661])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-07-17"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(662:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[662:1303])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-07-17"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-07-17"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-07-17"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-07-17"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-07-17"]][["contraste"]][[i]])
}

#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio

#2007-08-09
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:678)){
    precrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:678])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-08-09"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(679:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[679:1303])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-08-09"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-08-09"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-08-09"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-08-09"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-08-09"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-09-03
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:695)){
    precrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:695])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-09-03"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(696:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[696:1303])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-09-03"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-09-03"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-09-03"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-09-03"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-09-03"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio

#2007-10-01
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:715)){
    precrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:715])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-01"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(716:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[716:1303])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-01"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-01"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-01"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-01"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-01"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-10-30
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:736)){
    precrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:736])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-30"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(737:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[737:1303])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-30"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-30"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-30"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-30"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-10-30"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio


#2007-12-06
names <- colnames(rend_zoo)
tt <- rownames(data.frame(rend_zoo))
precrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(1:763)){
    precrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(precrisis) <- dput(names[2:10])
rownames(precrisis) <- dput(tt[1:763])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-12-06"]][["precrisis"]] <- precrisis

postcrisis <- data.frame()
xcounter <- 1
for(i in names[2:10]){
  icounter <- 1
  for (x in c(764:1303)){
    postcrisis[icounter,xcounter] <-rcor(dccfitdiag, type="R")[,,c(tt[x])]["USA_Index",i]
    icounter <- icounter + 1
  }
  xcounter <- xcounter + 1
}
colnames(postcrisis) <- dput(names[2:10])
rownames(postcrisis) <- dput(tt[764:1303])
listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-12-06"]][["postcrisis"]] <- postcrisis

for(i in names[2:10]){
  listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-12-06"]][["contraste"]][[i]] <- t.test(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-12-06"]][["precrisis"]][[i]],
                                                                                                      listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-12-06"]][["postcrisis"]][[i]],
                                                                                                      var.equal = FALSE)
  print(listcontraste[["SELEC DIAGONAL DCC T-Student (1,1)"]][["2007-12-06"]][["contraste"]][[i]])
}
#H0: mu1 - mu2 = 0; mu1 = mu2; no hay efecto contagio

