#Postworks
#Trabajos para el curso de Data Science de BEDU; modulo de R

#Postwork 1####
#Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R, los datos los puedes 
#encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
datos_PW1 = read.csv(file = "https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

#Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados por 
#los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)
library(dplyr)
datos_filtrados_PW1 = select(datos, c("FTHG", "FTAG"))

#Consulta cómo funciona la función table en R al ejecutar en la consola ?table
?table
#Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
freq_rel_PW1 = table(datos_filtrados)
  
#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
round(apply(freq_rel, 1, sum)/sum(freq_rel),3)
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
round(apply(freq_rel, 2, sum)/sum(freq_rel),3)
#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y 
#goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
round(freq_rel/sum(freq_rel),3)


#Postwork 2####
#Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R,
# los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
urls = c("https://www.football-data.co.uk/mmz4281/1718/SP1.csv",
         "https://www.football-data.co.uk/mmz4281/1819/SP1.csv",
         "https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
datos = lapply(urls,read.csv)
#Obten una mejor idea de las características de los data frames al usar las funciones: str, head, View y summary
summary(datos)

#Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; 
#esto para cada uno de los data frames. (Hint: también puedes usar lapply).
datos_filtrados = lapply(datos, select, "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG" ,"FTR")

#Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo 
#(Hint 1: usa as.Date y mutate para arreglar las fechas). Con ayuda de la función rbind forma un único data frame que contenga 
#las seis columnas mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).
datos_filtrados = do.call(rbind, datos_filtrados)
str(datos_filtrados)
datos_filtrados = mutate(datos_filtrados, Date = as.Date(Date, "%d/%m/%y"))

#Postwork 3 ####
#Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas para estimar las 
#siguientes probabilidades:
#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
freq_rel = select(datos_filtrados, c("FTHG","FTAG")) %>% table()
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)
visitante_probs = data.frame(Goles_anotados = 0:(dim(freq_rel)[2]-1),
                             Probabilidad_marginal = apply(freq_rel, 2, sum)/sum(freq_rel)) 

#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)
casa_probs = data.frame(Goles_anotados = 0:(dim(freq_rel)[1]-1),
                             Probabilidad_marginal = apply(freq_rel, 1, sum)/sum(freq_rel)) 

#Realiza lo siguiente:
#Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo de casa
library(ggplot2)
ggplot(casa_probs, aes(x = Goles_anotados, y = Probabilidad_marginal))+
  geom_bar(stat = "identity") + ggtitle("Probabilidades del equipo de casa")+
  theme_classic() + xlab("Goles anotados")+ ylab("Probabilidad marginal")

#Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo visitante.
ggplot(visitante_probs, aes(x = Goles_anotados, y = Probabilidad_marginal))+
  geom_bar(stat = "identity") + ggtitle("Probabilidades del equipo visitante")+
  theme_classic()+ xlab("Goles anotados")+ ylab("Probabilidad marginal")

#Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el equipo de casa y el equipo 
#visitante en un partido.
heatmap(freq_rel/sum(freq_rel), Rowv = NA, Colv = NA, 
        scale = "none", main = "Probabilidades conjuntas de anotar goles",
        xlab = "Equipo visitante", ylab = "Equipo de casa")

#Postwork 4####
# Investigaras la dependencia o independencia del número de goles anotados por el equipo de casa y el numero de goles anotados
# por el equipo visitante mediante un procedimiento denominado bootstrap
# Ya hemos estimado las probabilidades conjuntas del equipo de casa y visitante en un partido. Obtén una tabla de cocientes al
# dividir estas probabilidades conjuntas por el producto de las probabilidades marginales correspondientes
producto_prob_marg = matrix(casa_probs$Probabilidad_marginal) %*% c(visitante_probs$Probabilidad_marginal) #Obtener la matriz de productos de probabilidades marginales
dimnames(producto_prob_marg) = list(FTHG = 0:8, FTAG = 0:6) #Poner nombre a las dimensiones
tab_cocientes = (freq_rel/sum(freq_rel)) * producto_prob_marg #Calcular tabla de cocientes

#Mediante un procedimiento de boostrap, obtén más cocientes similares a los obtenidos en la tabla del punto anterior. Esto para 
#tener una idea de las distribuciones de la cual vienen los cocientes en la tabla anterior. 

library(reshape2)
library(ggplot2)
m.melted<-melt(tab_cocientes)
ggplot(m.melted, aes(x=FTHG,y=FTAG)) +
  geom_raster(aes(fill=value)) +
  #scale_fill_gradient(low="blue", high="red") +
  labs(x="FTHG", y="FTAG", title="Probabilidad normal")
library(rsample)
computos_boot <- bootstraps(m.melted, times = 100)
computos_boot
ggplot(computos_boot$splits[[100]]$data, aes(x=FTHG,y=FTAG)) +
  geom_raster(aes(fill=value)) +
  #scale_fill_gradient(low="blue", high="red") +
  labs(x="FTAG", y="FTHG", title="Probabilidad normal")

#Menciona en cuáles casos le parece razonable suponer que los cocientes de la tabla en el punto 1, son iguales a 1 (en tal caso 
#tendríamos independencia de las variables aleatorias X y Y).

#Postwork 5 ####
#A partir del conjunto de datos de soccer de la liga española de las temporadas 2017/2018, 2018/2019 y 2019/2020, crea el data 
#frame SmallData, que contenga las columnas date, home.team, home.score, away.team y away.score; esto lo puede hacer con ayuda 
#de la función select del paquete dplyr. Guarda el data frame como un archivo csv con nombre soccer.csv; row.names = FALSE en write.csv.
SmallData = lapply(datos, select, "Date", "HomeTeam", "HS", "AwayTeam", "AS") 
SmallData[[1]] = mutate(SmallData[[1]], Date = as.Date(Date,"%d/%m/%y"))# Cambiar el formato de fecha
SmallData[[2]] = mutate(SmallData[[2]], Date = dmy(Date))
SmallData[[3]] = mutate(SmallData[[3]], Date = dmy(Date))
SmallData = do.call(rbind,SmallData) #Crear Small Data

colnames(SmallData) = c("date", "home.team", "home.score", "away.team", "away.score") # Cambiar el nombre de las columnas como lo pide fbRanks
write.csv(SmallData, file = "soccer.csv", row.names = F) #Guardar el archivo csv
#Con la función create.fbRanks.dataframes del paquete fbRanks importe el archivo soccer.csv a R y al mismo tiempo asignelo a 
#una variable llamada listasoccer. Se creará una lista con los elementos scores y teams que son data frames listos para la 
#función rank.teams. Asigna estos data frames a variables llamadas anotaciones y equipos.
#install.packages("fbRanks")
library(fbRanks)
listasoccer = create.fbRanks.dataframes(scores.file = "soccer.csv")

#Con ayuda de la función unique crea un vector de fechas (fecha) que no se repitan y que correspondan a las fechas en las que se
# jugaron partidos. Crea una variable llamada n que contenga el número de fechas diferentes. Posteriormente, con la función 
# rank.teams y usando como argumentos los data frames anotaciones y equipos, crea un ranking de equipos usando unicamente datos
# desde la fecha inicial y hasta la penúltima fecha en la que se jugaron partidos, estas fechas las deberá especificar en
#  max.date y min.date. Guarda los resultados con el nombre ranking.
fecha = unique(listasoccer$scores$date)
n = length(fecha)
ranking = rank.teams(scores = listasoccer$scores, teams = listasoccer$teams, min.date = fecha[1], max.date = fecha[n-1])

#Finalmente estima las probabilidades de los eventos, el equipo de casa gana, el equipo visitante gana o el resultado es un 
# empate para los partidos que se jugaron en la última fecha del vector de fechas fecha. Esto lo puedes hacer con ayuda de la
# función predict y usando como argumentos ranking y fecha[n] que deberá especificar en date.

predicciones = filter(SmallData, date == fecha[n])#Ver los partidos jugados en la ultima fecha
#Generar las predicciones
for (partido in 1:nrow(predicciones)){
  predict(ranking, newdata=list(home.team=predicciones$home.team[partido], away.team=predicciones$away.team[partido]))
}

#Postwork 6####
#Importa el conjunto de datos match.data.csv a R y realiza lo siguiente:
match_data = read.csv("./Programacion-con-R-Santander-master/Sesion-06/Postwork/match.data.csv")
#Agrega una nueva columna sumagoles que contenga la suma de goles por partido.
match_data$sumagoles = match_data$home.score + match_data$away.score #Crear la columna sumagoles
match_data= mutate(match_data, date = as.Date(date))
#Obtén el promedio por mes de la suma de goles.
install.packages("lubridate")
library(lubridate)
match_data = mutate(match_data, Year = year(date), Month = month(date)) #Agregar una columna para año y otra para mes
df <- group_by(match_data, Year, Month) #Agrupar por mes y año
df <- summarise(df, result = mean(sumagoles) ) #Sacar el promedio de goles

#@@@@Hacer esta parte en caso de que quieras completar los datos de los meses en los que no hay partidos
#df = mutate(df, Date = dmy(paste("01/",Month, "/", Year, sep = "")))#Generar una columna nueva en formato de fecha
#Completar los registros incompletos
#tiempo_completo = data.frame(Date = seq.Date(min(df$Date), max(df$Date), by="month")) #Crear fechas mes por mes
#df = full_join(tiempo_completo, df) #Completar las fechas faltantes con el join 
#df$result[is.na(df$result)] = 0
#serie = ts(df$result, frequency = 12, end = c(2020,07, 01))
#@@@@@@@
#Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.
df = subset(df, Year < 2020)
serie = ts(df$result, frequency = 10, end = c(2019,12))
#Grafica la serie de tiempo.
plot(serie)
#d = decompose(serie, type = "additive")
#plot(d)

#Postwork 7 ####
#Utilizando el manejador de BDD Mongodb Compass (previamente instalado), deberás de realizar las siguientes acciones:
#Alojar el fichero data.csv en una base de datos llamada match_games, nombrando al collection como match
install.packages('mongolite')
library(mongolite)
#Sustituir PASSWORD por contraseña
URL <- sprintf("mongodb+srv://%s:%s@%s/", 'enavarro', PASSWORD, 'bedu-ds.qcd3k.mongodb.net')
#m = mongo(url = "mongodb+srv://enavarro:<PASSWORD>@bedu-ds.qcd3k.mongodb.net", 
#          collection = "match", db = "match_games")
col.match <- mongo("match", db = "match_games",url=URL)
col.match$drop() # Borrar datos de la colección
col.match$count() # Contar para asegurar que está vacía
datos <- read.csv('./Programacion-con-R-Santander-master/Sesion-07/Postwork/data.csv')
head(datos)
col.match$insert(datos) #Subir los datos

#Una vez hecho esto, realizar un count para conocer el número de registros que se tiene en la base
col.match$count() 

# Realiza una consulta utilizando la sintaxis de Mongodb, en la base de datos para conocer el número de goles que metió el Real
# Madrid el 20 de diciembre de 2015 y contra que equipo jugó, ¿perdió ó fue goleada?
col.match$find(
  query = '{"Date" : "2015-12-20", "HomeTeam" : "Real Madrid"}', 
  fields = '{"Date" : true, "AwayTeam" : true, "FTHG" : true, "FTAG":true}')  
#No se encuentra ningun partido en esa fecha

# Hacer otra busqueda de partidos del real madrid
col.match$find(
  query = '{"HomeTeam" : "Real Madrid"}', 
  fields = '{"Date" : true, "AwayTeam" : true, "FTHG" : true, "FTAG":true}') 
#Se observa que los datos para los partidos estan desde el 2017, entonces no se puede saber de partidos del 2015.
# Por último, no olvides cerrar la conexión con la BDD
  