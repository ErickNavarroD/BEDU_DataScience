#Reto 1 
#Queries
#Una vez hecha la conexión a la BDD, generar una busqueda con dplyr que devuelva el porcentaje de personas que hablan español en todos los países
#Realizar una gráfica con ggplot que represente este porcentaje de tal modo que en el eje de las Y aparezca el país y en X el porcentaje, y que diferencíe
#entre aquellos que es su lengua oficial y los que no con diferente color (puedes utilizar la geom_bin2d() y coord_flip())
#Una vez hecho esto hacer el commit y push para mandar tu archivo al repositorio de Github Reto_Sesion_7

install.packages("DBI")
install.packages("RMySQL")
install.packages("dplyr")
install.packages("ggplot2")

library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)

#Nos conectamos a la base de datos
MyDataBase <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest")

#Explorar la BDD
dbListTables(MyDataBase)

# Desplegar los campos o variables que contiene la tabla CountryLanguage
dbListFields(MyDataBase, 'CountryLanguage')

#Seleccionar los hablantes de español
DataDB <- dbGetQuery(MyDataBase, "select * from CountryLanguage where Language = 'Spanish'")

#Graficar
ggplot(DataDB, aes(x = CountryCode, y = Percentage))+
  geom_bar(stat = "identity") + 
  theme_classic() + 
  ggtitle("Porcentaje de hablantes de español por pais")
