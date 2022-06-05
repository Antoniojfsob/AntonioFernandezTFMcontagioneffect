#rm(list = ls())
#setwd()

################################### LIBRERÍAS #################################
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(readxl)
library(readr)
library(datetimeutils)
library(data.table)
library(lubridate)
library(glue)
library(openair)
library(reshape2)
library(PerformanceAnalytics)
library(corrplot)
library(corrgram)
library(gridExtra)
library(skimr)
library(Amelia)

################################ IMPORTACIÓN DE DATOS ##########################
#USA
usa <- read_csv("./DTWEXM.csv")
names(usa)[1] = "Date"
names(usa)[2] = "USA_Index"
usa$USA_Index <- as.numeric(usa$USA_Index)
head(usa)
tail(usa)

#MOROCCO
morocco <- read.csv("./USD_MAD Historical Data.csv")
names(morocco)[1] = "Date"
names(morocco)[2] = "MOR_Price"
head(morocco)
tail(morocco)
morocco$Date <- parse_date_time(morocco$Date, orders= "mdy")
morocco$Date <- as.Date(morocco$Date)
morocco <- morocco %>% dplyr::select(c(-Open,-High,-Low,-Change..))
morocco <- morocco %>% arrange(ymd(morocco$Date))

#SOUTH AFRICA
southafrica <- read.csv("./USD_ZAR Historical Data.csv")
names(southafrica)[1] = "Date"
names(southafrica)[2] = "SOUTHAFR_Price"
head(southafrica)
tail(southafrica)
southafrica$Date <- parse_date_time(southafrica$Date, orders= "mdy")
southafrica$Date <- as.Date(southafrica$Date)
southafrica <- southafrica %>% dplyr::select(c(-Open,-High,-Low,-Change..))
southafrica <- southafrica %>% arrange(ymd(southafrica$Date))

#EGIPTO 
egipto <- read.csv("./USD_EGP Historical Data.csv")
names(egipto)[1] = "Date"
names(egipto)[2] = "EGPT_Price"
head(egipto)
tail(egipto)
egipto$Date <- parse_date_time(egipto$Date, orders= "mdy")
egipto$Date <- as.Date(egipto$Date)
egipto <- egipto %>% dplyr::select(c(-Open,-High,-Low,-Change..))
egipto <-egipto %>% arrange(ymd(egipto$Date))

#GHANA 
ghana <- read.csv("./USD_GHS Historical Data.csv")
names(ghana)[1] = "Date"
names(ghana)[2] = "GHA_Price"
head(ghana)
tail(ghana)
ghana$Date <- parse_date_time(ghana$Date, orders= "mdy")
ghana$Date <- as.Date(ghana$Date)
ghana <- ghana %>% dplyr::select(c(-Open,-High,-Low,-Change..))
ghana <- ghana %>% arrange(ymd(ghana$Date))

#KENIA
kenia <- read.csv("./USD_KES Historical Data.csv")
names(kenia)[1] = "Date"
names(kenia)[2] = "KEN_Price"
head(kenia)
tail(kenia)
kenia$Date <- parse_date_time(kenia$Date, orders= "mdy")
kenia$Date <- as.Date(kenia$Date)
kenia <- kenia %>% dplyr::select(c(-Open,-High,-Low,-Change..))
kenia <- kenia %>% arrange(ymd(kenia$Date))

#NIGERIA 
nigeria <- read.csv("./USD_NGN Historical Data.csv")
names(nigeria)[1] = "Date"
names(nigeria)[2] = "NIG_Price"
head(nigeria)
tail(nigeria)
nigeria$Date <- parse_date_time(nigeria$Date, orders= "mdy")
nigeria$Date <- as.Date(nigeria$Date)
nigeria <- nigeria %>% dplyr::select(c(-Open,-High,-Low,-Change..))
nigeria <- nigeria %>% arrange(ymd(nigeria$Date))

#TUNISIA
tunisia <- read.csv("./USD_TND Historical Data.csv")
names(tunisia)[1] = "Date"
names(tunisia)[2] = "TUN_Price"
head(tunisia)
tail(tunisia)
tunisia$Date <- parse_date_time(tunisia$Date, orders= "mdy")
tunisia$Date <- as.Date(tunisia$Date)
tunisia <- tunisia %>% dplyr::select(c(-Open,-High,-Low,-Change..))
tunisia <- tunisia %>% arrange(ymd(tunisia$Date))

#ARGELIA
argelia <- read.csv("./USD_DZD Historical Data.csv")
names(argelia)[1] = "Date"
names(argelia)[2] = "ARG_Price"
head(argelia)
tail(argelia)
argelia$Date <- parse_date_time(argelia$Date, orders= "mdy")
argelia$Date <- as.Date(argelia$Date)
argelia <- argelia %>% dplyr::select(c(-Open,-High,-Low,-Change..))
argelia <- argelia %>% arrange(ymd(argelia$Date))

#TANZANIA
tanzania<- read.csv("./USD_TZS Historical Data.csv")
names(tanzania)[1] = "Date"
names(tanzania)[2] = "TAN_Price"
head(tanzania)
tail(tanzania)
tanzania$Date <- parse_date_time(tanzania$Date, orders= "mdy")
tanzania$Date <- as.Date(tanzania$Date)
tanzania$TAN_Price <- as.numeric(gsub(",","",tanzania$TAN_Price))
tanzania <- tanzania %>% dplyr::select(c(-Open,-High,-Low,-Change..))
tanzania <- tanzania %>% arrange(ymd(tanzania$Date))

#Juntarlos en el mismo dataframe
paises <- merge(argelia, egipto, all= TRUE)
paises <- merge(paises, ghana, all= TRUE)
paises <- merge(paises, kenia, all= TRUE)
paises <- merge(paises, morocco, all= TRUE)
paises <- merge(paises, nigeria, all= TRUE)
paises <- merge(paises, southafrica, all= TRUE)
paises <- merge(paises, tanzania, all= TRUE)
paises <- merge(paises, tunisia, all= TRUE)
paises <- merge(paises, usa, all= TRUE)
paises <- paises %>% relocate(USA_Index, .after = Date)
View(paises)

#################### ANÁLISIS EXPLORATORIO Y DEPURACIÓN DE LOS DATOS ###########

#Gráfico inicial
paises_melt <- melt(paises,id = "Date")
plot_list <- list() 
df <- split(paises_melt,paises_melt$variable)
for (i in seq_along(df)){
  plot_list[[i]] <- ggplot(df[[i]], aes(y = value, x = Date )) + 
    geom_line(lwd = 0.8)+ ylab("Valor") +
    scale_x_date("Fecha",date_breaks = "12 month", date_labels = "%m/%Y")+
    ggtitle(names(df)[i])
}
grid.arrange(grobs = plot_list, ncol = 2)

paises05_09 <- paises[paises$Date >= "2005-01-01" & paises$Date <= "2009-12-31",]
summary(paises05_09) 
skim(paises05_09)   
view(paises05_09)   


##TRATAMIENTO DE MISSINGS
length(which(is.na(paises05_09)))
length(which(!complete.cases(paises05_09))) 
missmap(paises05_09[2:11], main = "Missing Values", col = c("pink4", "snow2"),
        x.las = 1, x.cex=0.7)

#eliminación NAs
na <- rowSums(is.na(paises05_09))
plyr::count(na)
paises05_09_dep <- paises05_09[ rowSums(is.na(paises05_09)) != 9, ]
length(which(!complete.cases(paises05_09_dep))) 
summary(paises05_09_dep) 

#Imputación de los NAs restantes:
paises05_09_dep2 <- lapply(paises05_09_dep[-1],zoo::na.spline)
paises05_09_dep <- cbind(paises05_09_dep$Date, as.data.frame(paises05_09_dep2))
names(paises05_09_dep)[1] = "Date"
summary(paises05_09_dep)
view(paises05_09_dep)


##ATÍPICOS:
# x - media > 3*sd
means <- colMeans(paises05_09_dep[2:11])
meansdf <- as.data.frame(means)
meansdf
std <- lapply(paises05_09_dep[2:11],sd)
std

#Serie normal atípicos
names <- colnames(paises05_09_dep)
names <- names[2:11]
paises05_09_dep_melt <- melt(paises05_09_dep,id = "Date")
plot_list <- list() 
df <- split(paises05_09_dep_melt,paises05_09_dep_melt$variable)
for (i in seq_along(df)){
  mm <- meansdf[i,]
  ssd <- std[[i]]
  plot_list[[i]] <- ggplot(df[[i]], aes(y = value, x = Date )) + 
    geom_line(lwd = 0.8)+ ylab("Valor") +
    scale_x_date("Fecha",date_breaks = "12 month", date_labels = "%m/%Y")+
    ggtitle(names(df)[i])+ 
    geom_hline(yintercept = mm, col="darkblue", lty=2) +
    geom_hline(yintercept = mm + 3*ssd, col="darkred", lty=2) +
    geom_hline(yintercept = mm + 3*ssd, col="darkred", lty=2) +
    theme(text = element_text(size = 13)) 
}
grid.arrange(grobs = plot_list, ncol = 2)


#Atípicos rendimientos

#Creación rendimientos
paises05_09_dep_wdt <- paises05_09_dep %>% dplyr::select(-Date)
rend <- lapply(paises05_09_dep_wdt,function (x) diff(log(x)))
rend_df <- as.data.frame(rend)
rend_df <- cbind(paises05_09_dep$Date[-1],rend_df)
names(rend_df)[1] = "Date"
rend_df_wdt <- rend_df %>% dplyr::select(-Date)
summary(rend_df)
skim(rend_df)

#Gráficos
rend_means <- colMeans(rend_df_wdt)
rend_meansdf <- as.data.frame(rend_means)
rend_meansdf
rend_std <- lapply(rend_df_wdt,sd)
rend_std

rend_melt <- melt(rend_df,id = "Date")
plot_list <- list() 
df <- split(rend_melt,rend_melt$variable)
for (i in seq_along(df)){
  rmm <- rend_meansdf[i,]
  rssd <- rend_std[[i]]
  plot_list [[i]] <- ggplot(df[[i]], aes(y = value, x = Date )) + geom_line()+
    ylab("Rendimientos") + 
    scale_x_date("Fecha",date_breaks = "12 month", date_labels = "%m/%Y")+
    ggtitle(names(df)[i])+ 
    geom_hline(yintercept = rmm, col="darkblue", lty=2) +
    geom_hline(yintercept = rmm + 5*rssd, col="darkred", lty=2) +
    geom_hline(yintercept = rmm - 5*rssd, col="darkred", lty=2) 
}
grid.arrange(grobs = plot_list, ncol = 3)



###################### ANÁLISIS EXPLORATORIO TRAS DEPURAR  #####################

### POR SEPARADO ###

###AÑADIENDO LÍNEAs VERTICALES PARA DÍAS DE CRISIS
dates_vline <- as.Date(c("2007-07-17","2007-08-09",
                         "2007-09-03","2007-10-01",
                         "2007-10-30","2007-12-06"))
dates_vline <- which(paises05_09_dep$Date %in% dates_vline)

#Gráficos series con fechas de la crisis
paises05_09_melt <- melt(paises05_09_dep,id = "Date")
plot_list <- list() 
df <- split(paises05_09_melt,paises05_09_melt$variable)
for (i in seq_along(df)){
  plot_list[[i]] <- ggplot(df[[i]], aes( x= Date, y= value)) + geom_line()+
    geom_vline(xintercept = as.numeric(paises05_09_dep$Date[dates_vline]), linetype= 2, 
               size = 1, color =c ("red","black","orange","blue","green","purple")) +
    scale_y_continuous("Índice") + 
    scale_x_date("Fecha",date_breaks = "7 month", date_labels = "%d/%m/%Y")+
    ggtitle(names(df)[i])
}
grid.arrange(grobs = plot_list, ncol = 2, common.legend)


#Correlación:
matriz_corr <- corrgram(paises05_09_dep_wdt,
                        order = F, 
                        lower.panel = panel.cor,
                        upper.panel = panel.pts,
                        text.panel = panel.txt,
                        diag.panel = panel.minmax,
                        main = "Correlation")
matriz_corr["USA_Index",]
corrplot(matriz_corr, method = 'shade', tl.cex= 0.7)


#Representación gráfica de RENDIMIENTOS con líneas de crisis
for (i in names){
  x <- rend_df[,i]
  plots <- ggplot(rend_df, aes( x= Date, y= x)) + geom_line() +
    geom_vline(xintercept = as.numeric(rend_df$Date[dates_vline]), linetype= 2, 
               size = 1, color =c ("red","black","orange","blue","green","purple"))+
    scale_y_continuous("Rendimientos") + 
    scale_x_date("Fecha",date_breaks = "5 month", date_labels = "%d/%m/%Y")+
    ggtitle(glue("Rendimientos de {paste(i)}"))
  print(plots)
}


###################### Guardamos datos para otros scripts ######################
save(rend_df, file ="rend_df.rda")
save(rend_df_wdt, file ="rend_df_wdt.rda")
save(paises05_09_dep_wdt, file ="paises05_09_dep_wdt.rda")
