#Proyecto final de BEDU
#Modulo de R
#Equipo 25; Andres, Erick, Ricardo y Jose

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


####Evaluacion de emision de CO2 por pais####
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
