#Proyecto final de BEDU
#Modulo de R
#Equipo 25; Andres, Erick, Ricardo y Jose

####Modelo de prediccion de temperatura ####
rm(list = ls())

library(rvest)
library(dplyr)
library(ggplot2)
library(scales)
library (forecast)
library(astsa)

url <- "https://www.ncdc.noaa.gov/cag/global/time-series/globe/land_ocean/1/12/1880-2020/data.csv"

tdatosr <- read.csv(url)

summary(tdatosr)
head(tdatosr)

tdatos<-tdatosr[-(1:4),]

tdatos[1,]

tdatos<-mutate(tdatos,year=as.numeric(Global.Land.and.Ocean.Temperature.Anomalies), anomalies=as.numeric(December))
tdatos<-select(tdatos,year,anomalies)

plot(tdatos$year,tdatos$anomalies)

tdatos.ts<-ts(tdatos$anomalies,start=c(tdatos$year[1],1), frequency=1)

#### Análisis de Fourier --------------------------------------------------------------------

library(spectral)
tdatos.fft<-spec.fft(x=tdatos$year,y=tdatos$anomalies)

maximos<-sort(Mod(tdatos.fft$A),decreasing=TRUE)
plot(tdatos.fft,type="l",main="Espectro de la Frecuencia", xlab="Frecuencia", ylab="Mod(A)")
for (i in c(3,4,7,11,13,15,16)){
  indices<-which(Mod(tdatos.fft$A)==maximos[i])
  print("Periodo(s) en años")
  print(1/tdatos.fft$fx[indices])
  print("Porcentaje que representa de los datos")
  print((1/tdatos.fft$fx[indices])/141)
  abline(v=tdatos.fft$fx[indices], col="grey")
  text(x=tdatos.fft$fx[indices]*30*maximos[i],y=1.4*maximos[i],
       labels=as.character(round(1/tdatos.fft$fx[indices]),digits=3),col="blue",
       offset=0.5)
}
abline(v=0.0909)
abline(v=-0.0909)

tdatos.ts.solar<-ts(tdatos$anomalies,start=c(1,1), frequency=9.09)
tdatos.ts.solar.dc<-decompose(tdatos.ts.solar,type="additive")
plot(tdatos.ts.solar.dc, "Descomposición de la serie de tiempo")
acf(na.omit(tdatos.ts.solar.dc$random))
plot(tdatos.ts.solar,type="l", main="Descomposición aditiva para f=9.09", ylab="Desviación de la Temperatura [°C]", xlab="Numero de periodo")
lines(tdatos.ts.solar.dc$trend,col="red")
lines(tdatos.ts.solar.dc$seasonal+tdatos.ts.solar.dc$trend,col="blue")
lines(tdatos.ts.solar.dc$seasonal+tdatos.ts.solar.dc$trend+tdatos.ts.solar.dc$random,col="green",type="p")


tdatos.solar.fft<-spec.fft(as.numeric(na.omit(tdatos.ts.solar.dc$trend)))
plot(na.omit(tdatos.solar.fft),type="l", main="Espectro de la frecuencia de la tendencia",
     xlab="Frecuencia", ylab="Mod(A)")

tdatos.solar.fft<-spec.fft(as.numeric(na.omit(tdatos.ts.solar.dc$seasonal)))
plot(na.omit(tdatos.solar.fft),type="l", main="Espectro de la frecuencia de la componenete estacional",
     xlab="Frecuencia", ylab="Mod(A)")

tdatos.solar.fft<-spec.fft(as.numeric(na.omit(tdatos.ts.solar.dc$random)))
plot(na.omit(tdatos.solar.fft),type="l", main="Espectro de la frecuencia para el ruido",
     xlab="Frecuencia", ylab="Mod(A)")

tdatos.solar.random.ar<-arima(as.numeric(na.omit(tdatos.ts.solar.dc$random)),c(0,0,0))
acf(resid(tdatos.solar.random.ar))

#### Regresión polinomial -----------------------------------------------------------------
plot(tdatos.ts, col="grey", main="Regresión polinomial de la variación de la temperatura 1880-2020", ylab="Desviación de la temperatura [°C]",
     xlab="Año")
lines(lowess(tdatos.ts,f=0.0298,iter=3),col="blue") # 4.2 años
lines(lowess(tdatos.ts,f=0.0645,iter=3),col="blue") # 9.096 años
lines(lowess(tdatos.ts,f=0.1052,iter=3),col="red") # 14.84 años
lines(lowess(tdatos.ts,f=0.1333,iter=3),col="red") # 18.8 años
lines(lowess(tdatos.ts,f=0.4000,iter=3),col="black") # 56.4 años
text(c(1890,1890,1890),c(1,0.8,0.6),labels=c("f=9.09","f=14.84","f=56.4"),col=c("blue","red","black"),cex=1.5)

#### Modelo ARIMA ----------------------------------------------------------------------
acf(tdatos.ts) # d=1
pacf(tdatos.ts) # q=2

tdatos.ar<-arima(tdatos.ts,c(0,1,2))
tdatos.ar$aic

best.order <- c(0, 0, 0)
best.aic <- Inf
for(i in 0:2)for(j in 0:2){
  model <- arima(tdatos.ts, order = c(i, 1, j))
  fit.aic <- AIC(model)
  if(fit.aic < best.aic){
    best.order <- c(i, 1, j)
    best.arma <- arima(tdatos.ts, order = best.order)
    best.aic <- fit.aic
  }
}

best.order
best.aic

tdatos.ar<-arima(tdatos.ts,best.order)
tdatos.ar$aic

acf(resid(tdatos.ar))
pacf(resid(tdatos.ar))

#### Predicción -------------------------------------------------------------------------------
tdatos.pred<-forecast(tdatos.ts,h=10,level=0.954,robust=TRUE,model=tdatos.ar,biasadj = TRUE)
ts.plot(tdatos.pred)

ts.plot(cbind(tdatos.pred$x,tdatos.pred$mean,tdatos.pred$lower,tdatos.pred$upper),col=c("blue","red","grey","grey"),
        gpars=list(xlab="Año",ylab="Variación de la Temperatura [°C]", main="Predicción a 10 años con un modelo ARIMA(0,1,1)"))
abline(h=0)

#### Modelo SARIMA y predicción -----------------------------------------------------------------
tdatos.sar<-sarima(tdatos.ts,p=0,d=1,q=1,P=2,D=0,Q=0,S=9.09)
tdatos.sar$AIC*141
acf(tdatos.sar$fit$residuals)
pacf(tdatos.sar$fit$residuals)
tdatos.pred<-sarima.for(tdatos.ts, n.ahead=100, p=0, d=1, q=1, P=2, D=0, S=9.09, plot=TRUE, plot.all=TRUE)

tdatos.sar$ttable
tdatos.sar$fit

tdatos.pred2015<-sarima.for(tdatos.ts[1:136], n.ahead=5, p=0, d=1, q=1, P=2, D=0, S=9.09, plot=TRUE, plot.all=TRUE)
lines(c(136:141), tdatos.ts[136:141], col="blue")

tdatos.pred2010<-sarima.for(tdatos.ts[1:131], n.ahead=10, p=0, d=1, q=1, P=2, D=0, S=9.09, plot=TRUE, plot.all=TRUE)
lines(c(131:141), tdatos.ts[131:141], col="blue")

tdatos.pred1920<-sarima.for(tdatos.ts[1:101], n.ahead=40, p=0, d=1, q=1, P=1, D=0, S=9.09, plot=TRUE, plot.all=TRUE)
lines(c(101:141), tdatos.ts[101:141], col="blue")

#### Prueba de Hipótesis ----------------------------------------------------------------------
plot(tdatos.ts.solar.dc$trend)
plot(c(1881:2020),diff(tdatos.ts.solar.dc$trend),type="l", main="Aumento anual de temperatura",
     ylab="T2-T1 [°C]",xlab="Año")

tdatos.ts.low<-lowess(tdatos.ts,f=0.0645,iter=2)

boxplot(cbind(tdatos.ts.solar.dc$trend[1:132],tdatos.ts.solar.dc$trend[133:138]))
abline(h=0)

#### 10 años ---------------------------------
boxplot(cbind(tdatos.ts[1:131],tdatos.ts[131:141]),main="Variación de la temperatura 1880-2010 (1) vs 2011-2020 (2)",
        ylab="Temperatura [°C]")
abline(h=0)

boxplot(cbind(tdatos.ts.low$y[1:131],tdatos.ts.low$y[131:141]), main="Variación de la temperatura 1880-2010 (1) vs 2011-2020 (2) del modelo regresivo f=9.09",
        ylab="Temperatura [°C]")
abline(h=0)

boxplot(cbind(diff(tdatos.ts[1:131]),diff(tdatos.ts[131:141])), main="Diferencia de temperatura 1880-2010 (1) vs 2011-2020 (2)",
        ylab="Temperatura [°C]")
abline(h=0)

boxplot(cbind(diff(tdatos.ts.low$y[1:131]),diff(tdatos.ts.low$y[131:141])), main="Diferencia de temperatura 1880-2010 (1) vs 2011-2020 (2) del modelo regresivo f=9.09",
        ylab="Temperatura [°C]")
abline(h=0)

plot(tdatos.ts,type="l")
lines(tdatos.ts.low,col="red")

t.test(diff(tdatos.ts[131:141]),mu=0)
t.test(diff(tdatos.ts.low$y[131:141]),mu=0,alternative="g")
t.test(tdatos.ts.low$y[131:141],mu=0,alternative="g")

#### 5 años ---------------------------------
boxplot(cbind(tdatos.ts[1:135],tdatos.ts[136:141]))
abline(h=0)

boxplot(cbind(tdatos.ts.low$y[1:135],tdatos.ts.low$y[136:141]))
abline(h=0)

boxplot(cbind(diff(tdatos.ts[1:135]),diff(tdatos.ts[136:141])))
abline(h=0)

boxplot(cbind(diff(tdatos.ts.low$y[1:135]),diff(tdatos.ts.low$y[136:141])))
abline(h=0)

plot(tdatos.ts,type="l")
lines(tdatos.ts.low,col="red")

t.test(diff(tdatos.ts[136:141]),mu=0)
t.test(diff(tdatos.ts.low$y[136:141]),mu=0,alternative="g")
t.test(tdatos.ts.low$y[136:141],mu=0,alternative="g")
abline(h=0.876)

#### Modelo lineal de CO2 y temperatura ----------------------------------------------------------------------------------
url2<-"https://www.esrl.noaa.gov/gmd/webdata/ccgg/trends/co2/co2_annmean_gl.txt"
cdatosr<-read.delim(url2,skip=56,sep="")
str(cdatosr)
head(cdatosr$X)

cdatos<-select(mutate(cdatosr, unc=mean, mean=year, year=X.),year,mean,unc)
str(cdatos)

#### Modelo Lineal
Temperatura.lm<-lm(tdatos$anomalies[101:140]~cdatos$mean)
summary(Temperatura.lm)
plot(Temperatura.lm)

plot(tdatos$anomalies[101:140]~cdatos$mean, main="Diagrama de Dispersión", xlab="Concentración de CO2 [ppm]", ylab="Desviación de la temperatura [°C]")
lines(cdatos$mean, coef(Temperatura.lm)[1]+coef(Temperatura.lm)[2]*cdatos$mean)


####Evaluacion de emision de CO2 por pais####
#Importar el dataset
co2_data = read.csv("https://github.com/owid/co2-data/raw/master/owid-co2-data.csv") 

#Seleccinar las variables de interes
#iso_code = codigo del pais de 3 letras
#co2 = Annual production-based emissions of carbon dioxide (CO2), measured in million tonnes per year.
#co2_growth_prct = Percentage change in CO2 emissions from one year relative to the previous year.
#co2_growth_abs =  Annual change in CO2 emissions from one year relative to the previous year, measured in million tonnes.
#co2_per_capita = Average per capita CO2 emissions, measured in tonnes per year.
#share_global_co2 = National or regional annual CO2 emissions, measured as a percentage of the global total
#total_ghg = total_ghg	Annual greenhouse gas emissions, measured in million tonnes of carbon dioxide equivalents.
#ghg_per_capita = Greenhouse gas emissions per capita, measured in tonnes of carbon dioxide equivalents.

library(tidyverse)
co2_data = co2_data %>% select(c(iso_code,
                                          country,
                                          year, 
                                          co2,
                                          co2_growth_prct,
                                          co2_growth_abs,
                                          co2_per_capita,
                                          share_global_co2))

#Crear lista de paises que no estan adscritos al acuerdo de Paris para el 2018 
#Fuentes: https://www.climatechangenews.com/2020/08/13/countries-yet-ratify-paris-agreement/
#https://treaties.un.org/pages/ViewDetails.aspx?src=TREATY&mtdsg_no=XXVII-7-d&chapter=27&clang=_en
no_paris_agree = data.frame(country = c("Iran","Iraq", "Libya", "Yemen", "South Sudan", "Angola", "Turkey", "Russia", "Eritrea", "Nicaragua",
                                  "Oman", "Suriname", "Kyrgyzstan", "Lebanon", "Burundi", "Colombia", "Trinidad and Tobago", "Tanzania",
                                  "Uzbekistan", "Equatorial Guinea", "Guinea-Bissau", "Kuwait", "Liberia", "Mozambique", "North Macedonia"))
#Crear lista de elementos en country que no son paises
no_paises_lista = c("Africa", "Asia", "Asia (excl. China & India)", "EU-27", "EU-28","Europe (excl. EU-27)", "Europe (excl. EU-28)",
                    "KP Annex B", "Kuwait Oil Fires", "Non KP Annex B", "Non-OECD", "North America", "North America (excl. USA)",
                    "OECD", "Oceania", "Reunion", "Sint Maarten (Dutch part)", "South America", "Statistical Difference",
                    "USSR", "World", "Europe","Antarctic Fisheries" )

#Filtrar elementos que no son paises
datos_mundiales = subset(co2_data, country == "World") #Guardar los datos mundiales 
co2_data = co2_data %>% subset(.,!(country %in% no_paises_lista))
#Agregar columna que diga si son miembros del acuerdo de paris a inicios del 2018
co2_data$Paris_agreement = !(co2_data$country %in% no_paris_agree$country)
co2_data$post_paris = co2_data$year > 2015
#Seleccionar datos de después de la revolucion industrial 
co2_data = subset(co2_data, year > 1849)

#Crear df con el nombre de cada pais
paises = unique(subset(co2_data, select = c("country","iso_code")))

#Verificar que no haya NAs en los datos que vamos a usar
sum( is.na( co2_data$co2_growth_abs ) ) > 0

#Generar data frame resultado con el p.value de la prueba de si el cambio anual de los ultimos años es menor al que se ha 
#tenido desde la revolucion indistrial 
resultado = data.frame()
for (pais in paises$country){
  datos_pais = subset(co2_data, co2_data$country == pais)
  pre_paris = subset(datos_pais, post_paris == F)
  post_paris = subset(datos_pais, post_paris == T)
  tryCatch({
    res_pval = t.test(pre_paris$co2_growth_abs, post_paris$co2_growth_abs, alternative = "greater")$p.value
    res_pais = data.frame(country = pais, p.val_co2 = res_pval)
    resultado = rbind(resultado, res_pais)},error=function(e){})
}

#Agregar columna que indique si la disminucion es significativa
resultado$disminucion = resultado$p.val_co2 < 0.05

#Generar data frame con los datos totales
co2_data = subset(co2_data, country %in% resultado$country)
co2_data = right_join(co2_data, resultado, by = "country")


#Graficar curvas de Co2
library(ggplot2)
colnames(co2_data)[1] = "iso_a3"
dis.labs <- c("Tendencia postAP +/o", "Tendencia postAP -")
names(dis.labs) <- c("FALSE", "TRUE")

ggplot(co2_data, aes(x = year, y = co2 ,group = country ,colour = Paris_agreement, label = iso_a3))+
  geom_line() + 
  geom_label(data = subset(co2_data, year == 1995)) + #Agregar etiquetas en posicion de 1995
  facet_wrap("disminucion", labeller = labeller(disminucion = dis.labs)) + #Dividir entre tendencias
  theme_bw() + ggtitle("Emisión de CO2 desde la RI")+
  xlab("Año")+ ylab("CO2 (mill. toneladas)") +
  scale_color_discrete(name = "¿Están en el Acuerdo de París?", labels = c("No", "Si"))

paises_descremento = subset(co2_data, co2_data$disminucion ==T)


#Graficar el mapa
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
install.packages("rgeos")

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
library("sf")
library("ggrepel")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

world = left_join(world, subset(co2_data, year =="2018"), by = "iso_a3")
ggplot(data = world) +
  geom_sf(aes(fill = disminucion)) + theme_bw() + 
  geom_label_repel(data= subset(world_points, world_points$iso_a3 %in% unique(paises_descremento$iso_a3)),aes(x=X, y=Y, label=name),
                                                            color = "darkblue", fontface = "bold")+
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Tendencia anual de cambio postAP")+ 
  scale_fill_discrete(name = "Tendencia de cambio postAP", labels = c("+/o", "-", "Sin Datos"))


#Hacer prueba para ver si el tratado funciono
#Ver paises con disminucion y que esten en el tratado de paris
nrow(subset(resultado, disminucion ==T & !(country %in% no_paris_agree$country)))
#Ver paises sin disminucion y que esten en el tratado de paris
nrow(subset(resultado, disminucion ==F & !(country %in% no_paris_agree$country)))
#Ver paises con disminucion y que no esten en el tratado de paris
nrow(subset(resultado, disminucion ==T & country %in% no_paris_agree$country))
#Ver paises sin disminucion y que no esten en el tratado de paris
nrow(subset(resultado, disminucion ==F & country %in% no_paris_agree$country))

#Se hace la tabla de contingencia
tab_cont = matrix(c(180,25,9,0),2,2,
                  dimnames = list(AcuerdoParis = c("si","no"),
                                  Disminucion = c("+/o","-")))
tab_cont
#Prueba estadistica para ver independencia
fisher.test(tab_cont)
