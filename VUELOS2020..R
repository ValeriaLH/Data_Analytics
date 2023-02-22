library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

###################################################################
################### IMPORTAR BASE DE DATOS ########################
###################################################################

Vuelos2020 <- read.csv("Jan_2020_ontime.csv")
Vuelos2020

###################################################################
################### LIMPIEZA DE BASE DE DATOS #####################
###################################################################

#------------------- BORRAR VARIABLE x -------------------#
Vuelos2020$X <- NULL

#--------------------- ENCONTRAR NA ----------------------#
sapply(Vuelos2020, function(Vuelos2020)sum(is.na(Vuelos2020)))
## se encontraron NA en: DEP_TIME, DEP_DEL15, ARR_TIME, ARR_DEL15. 
## (OBS actuales: 607346)

#---------------------- SOLUCIONAR NA --------------------#
Vuelos2020 <- Vuelos2020[Vuelos2020$ARR_DEL15!="NA",]
sapply(Vuelos2020, function(Vuelos2020)sum(is.na(Vuelos2020)))
Vuelos2020 <- na.omit(Vuelos2020)
sapply(Vuelos2020, function(Vuelos2020)sum(is.na(Vuelos2020)))

###################################################################
################## MANIPULACION BASE DE DATOS #####################
###################################################################

#--- SE CUENTAN EL NUMERO DE ORIGENES PRESENTE EN LA BASE DE DATOS ---#
Vuelos_origen <- Vuelos2020 %>%
  group_by(ORIGIN)%>%
  count(ORIGIN)


## Ranking de mayor a menor de ciudades con mas de 15000 vuelos de origen en el mes ##  
Vuelos_origen_filter <- arrange(filter(Vuelos_origen, n > 15000), desc(n))
Vuelos_origen_filter1 <- arrange(filter(Vuelos_origen, n < 20), desc(n))

#--- SE CUENTAN EL NUMERO DE DESTINOS PRESENTE EN LA BASE DE DATOS ---#
Vuelos_destino <- Vuelos2020 %>%
  group_by(DEST)%>%
  count(DEST)

## Ranking de ciudades con mas de 20000 vuelos de origen en el mes ##
Vuelos_destino_filter <- filter(Vuelos_destino, n > 20000)


#--- SE CUENTAN CUANTOS VUELOS EXISTIERON CADA DIA DE LA SEMANA ---#
Vuelos_semana <- Vuelos2020 %>%
  group_by(DAY_OF_WEEK)%>%
  count(DAY_OF_WEEK)


#--- SE CUENTAN CUANTOS VUELOS EXISTIERON CADA DIA DEL MES ---#
Vuelos_mes <- Vuelos2020 %>%
  group_by(DAY_OF_MONTH)%>%
  count(DAY_OF_MONTH)

## Se definen los dias del mes en los cuales existieron mas de 20 mil vuelos ##
Vuelos_mes_filter <- filter(Vuelos_mes, n > 20500)


#--- SE CUENTAN EL NUMERO DE VUELOS DE ACUERDO AL INTERVALO DE HORA ---#
Vuelos_hora <- Vuelos2020 %>%
  group_by(DEP_TIME_BLK)%>%
  count(DEP_TIME_BLK)

Vuelos_hora_filter <- arrange(filter(Vuelos_hora, n > 37500), desc(n))
Vuelos_hora_filter <- arrange(filter(Vuelos_hora, n < 5000))


#--- VUELOS CON RETRASO DESDE EL ORIGEN ---#   
Vuelos_retraso <- Vuelos2020 %>%
  group_by(DEP_DEL15)%>%
  count(DEP_DEL15)      

#--- VUELOS CON RETRASO AL ARRIBO ---#
Vuelos_retraso <- Vuelos2020 %>%
  group_by(ARR_DEL15)%>%
  count(ARR_DEL15)      

#--- VUELOS CANCELADOS ---#
Vuelos_cancelados <- Vuelos2020 %>%
  group_by(CANCELLED)%>%
  count(CANCELLED)   

#--- DISTANCIA ENTRE AEROPUERTOS ---#
distancia <- Vuelos2020 %>%
  group_by(DISTANCE)%>%
  count(DISTANCE) 

distancia_filter <- arrange(filter(distancia, n > 2350), desc(n))

#--- AVIONES CON MAS RECORRIDO ---#
aviones <- Vuelos2020 %>%
  group_by(TAIL_NUM)%>%
  count(TAIL_NUM)

aviones_filter <- arrange(filter(aviones, n > 292), desc(n))

###################################################################
###################### CREACION DE VARIABLES ######################
###################################################################

#--- SEPARAR HORAS DE MINUTOS TANTO EN HORARIO DE SALIDA COMO DE LLEGADA ---#

Vuelos2020_mutate <- mutate(transmute(Vuelos2020,
                                      DEP_TIME,
                                      hour_D = DEP_TIME %/% 100,
                                      minute_D = DEP_TIME %% 100),
                            transmute(Vuelos2020,
                                      ARR_TIME,
                                      hour_A = ARR_TIME %/% 100,
                                      minute_A = ARR_TIME %% 100))


#--- SE PASAN LOS MINUTOS A SU EQUIVALENTE A HORA ---#

Vuelos2020_mutate1 <- mutate(Vuelos2020_mutate, minuteD_to_hour = minute_D/60,
                             minuteA_to_hour = minute_A/60)

#--- SE SUMAN LAS HORAS CON EQUIVALENTES A HORAS ---#
Vuelos2020_mutate2 <- mutate(Vuelos2020_mutate1, FLIGHT_TIME = abs(((hour_D + minuteD_to_hour) -
                                                                      (hour_A + minuteA_to_hour))))



#--- SE PASA VARIABLE FLIGHT_TIME A DATASET ORIGINAL Y SE CREA VARIABLE SPEED ---#
#---                (velocidad) Y FLIGHT_ROUT (ruta de vuelo)                 ---#

Vuelos2020_Org <- mutate(Vuelos2020, FLIGHT_TIME = Vuelos2020_mutate2$FLIGHT_TIME,
                         SPEED = DISTANCE/FLIGHT_TIME,
                         FLIGHT_ROUT = paste(Vuelos2020$ORIGIN, Vuelos2020$DEST,sep = "-"))

ruta_de_vuelo <- Vuelos2020_Org %>%
  group_by(FLIGHT_ROUT)%>%
  count(FLIGHT_ROUT) 

ruta_vuelo_filter <- arrange(filter(ruta_de_vuelo, n>1000))


###################################################################
############################ GRAFICOS #############################
###################################################################

#--- GRAFICO RANKING DE AEROPUERTOS ORIGENES COMO MAS VUELOS ---#
ggplot(Vuelos_origen_filter, aes(x=ORIGIN, y=n)) +
  geom_bar(stat = "identity")


#--- GRAFICO DE VUELOS CADA DIA DE LA SEMANA ---#

dias <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
coul <- brewer.pal(7, "Set2")

barplot(height=Vuelos_semana$n, names=dias, 
        xlab="Días de la semana", 
        ylab="Número de viajes", 
        main="Número de viajes por día de semana", 
        ylim=c(0,104000),
        col=coul)


#--- GRAFICO AEROPUERTOS DE ORIGEN CON MAS VUELOS DE SALIDA ---#

origen <- c("ATL", "ORD","DFW", "DEN", "CLT", "LAX", "PHX")
coul <- brewer.pal(7, "Set2")

barplot(height=Vuelos_origen_filter$n, names=origen, 
        xlab="Aeropuertos", 
        ylab="Número de viajes", 
        main="Aerpuertos de Origen más populares", 
        ylim=c(0,32100),
        col=coul)

#--- GRAFICO AEROPUERTOS DE DESTINO CON MAS VUELOS DE LLEGADA ---#

destino <- c("ATL", "DEN","DFW", "ORD")
coul <- brewer.pal(4, "Set2")

barplot(height=Vuelos_destino_filter$n, names=destino, 
        xlab="Aeropuertos", 
        ylab="Número de viajes", 
        main="Aerpuertos de Destino más populares", 
        ylim=c(0,32100),
        col=coul)

#--- GRAFICO DE VUELOS CADA DIA DEL MES ---#

barplot(height=Vuelos_mes$n, names=Vuelos_mes$DAY_OF_MONTH, 
        xlab="Dia del mes", 
        ylab="Cantidad de viajes", 
        main="Cantidad de vuelos cada día del mes de enero", 
        ylim=c(0,21000))

hist(x = Vuelos_mes$n, main = "Histograma de Cantidad de Vuelos en el mes de enero", 
     xlab = "Cantidad de vuelos", ylab = "Frecuencia")

Dia_del_mes <- c(Vuelos_mes$DAY_OF_MONTH)
Cantidad_de_vuelos <- c(Vuelos_mes$n)

plot(Dia_del_mes, Cantidad_de_vuelos, type = "l",
     col = "red",
     lwd = 5,
     main = "Tráfico de vuelos durante enero 2020")


#--- GRAFICO RUTAS DE VUELO MAS POPULARES ----#

ruta <- c("JFK-LAX","LAS-LAX","LAX-JFK","LAX-LAS","LAX-SFO","LGA-ORD","ORD-LGA","SFO-LAX")
coul <- palette("Pastel 2")

barplot(height=ruta_vuelo_filter$n, names=ruta, 
        xlab="Rutas", 
        ylab="Cantidad de viajes", 
        main="Rutas más populares", 
        ylim=c(0,1200),
        col=coul)


distancia_filter <- arrange(filter(distancia, n > 2350), desc(n))

#--- GRAFICO AVIONES QUE HAN REALIZADO MAS DE 250 VIAJES ---#

aviones_1 <- c("N488HA", "N490HA","N494HA", "N477HA",
               "N484HA", "N483HA", "N480HA", "N489HA")
coul <- palette("Pastel 2")

barplot(height=aviones_filter$n, names=aviones_1, 
        xlab="Identificación de Aviones", 
        ylab="Cantidad de viajes", 
        main="Aviones que han realizado una mayor cantidad de viajes", 
        ylim=c(0,340),
        col=coul)

#--- GRAFICO AEROLINEAS MAS POPULARES ----#

aerolineas. <- c("WN","DL","AA","OO","UA","YX","MQ")
coul <- palette("Pastel 2")

barplot(height=aerolineas_filter$n, names=aerolineas., 
        xlab="Aerolíneas", 
        ylab="Cantidad de viajes", 
        main="Aerolíneas más populares", 
        ylim=c(0,110000),
        col=coul)

#--- GRAFICO VIAJES SEGUN INTERVALO HORARIO ---#

Vuelos_hora <- Vuelos2020_Org %>%
  group_by(DEP_TIME_BLK)%>%
  count(DEP_TIME_BLK)


barplot(height=Vuelos_hora$n, names=Vuelos_hora$DEP_TIME_BLK, 
        xlab="Intervalo Horario (ORIGEN)", 
        ylab="Cantidad de viajes", 
        main="Viajes según intervalo horario ", 
        ylim=c(0,44000))


###################################################################
##################### PREGUNTAS DE NEGOCIO ########################
###################################################################

##--- 1° ¿Los atrasos aumentan con la distancia? ---##

por_destino <- group_by(Vuelos2020_Org, DEST)
atraso <- summarise(por_destino,
                    conteo = n(),
                    distancia_millas = mean(DISTANCE, na.rm = TRUE),
                    atraso = mean(ARR_DEL15, na.rm = TRUE)
)

atraso1 <- filter(atraso, conteo > 9000)

# Las demoras aumentan entre las distancias 900 a aproximadamente 1200 millas

ggplot(data = atraso1, mapping = aes(x = distancia_millas, y = atraso)) +
  geom_point(aes(size = conteo), alpha = 1/3) +
  geom_smooth(se = FALSE)


##--- 3°¿Los retrasos dependen de factores temporales, es decir, la hora y el día de 
##--- salida de un vuelo, es determinante en los retrasos?

Vuelos_semana <- Vuelos2020 %>%
  group_by(DAY_OF_WEEK)%>%
  count(DEP_DEL15)

dias <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
atraso <- c(11125, 7526, 9519, 13879, 15703, 12403, 11292)

barplot(height=atraso, names=dias, 
        xlab="Dias de la semana", 
        ylab="Cantidad de atrasos", 
        main="Atrasos según dia de la semana ", 
        ylim=c(0,16000))


Vuelos_hora1 <- Vuelos2020 %>%
  group_by(DEP_TIME_BLK)%>%
  count(DEP_DEL15)

bloque_horario <- c("01:00-05:59","06:00-06:59","07:00-07:59","08:00-08:59","09:00-09:59",
                    "10:00-10:59","11:00-11:59","12:00-12:59","13:00-13:59","14:00-14:59",
                    "15:00-15:59","16:00-16:59","17:00-17:59","18:00-18:59","19:00-19:59",
                    "20:00-20:59","21:00-21:59","22:00-22:59","23:00-23:59")
atrasos_blk <- c(1191,2611,2969,3616,3931,4705,4930,5321,5064,5641,5764,5962,6690,6194,5925,
                 5043,3149,2112,629)

bloque <- c(1:19)

data <- data.frame(bloque,atrasos_blk)
                   
barplot(height=atrasos_blk, names=bloque_horario, 
        xlab="Bloques de horario", 
        ylab="Cantidad de atrasos", 
        main="Atrasos según bloque de horario ", 
        ylim=c(0,7000)) 

ggplot(data, aes(x=bloque, y=atrasos_blk)) +
  geom_line() +
  ggtitle("Evolución de Atrasos")

#--- 4° ¿Las rutas más populares (mayor número de vuelos) tienen mayor tasa de retraso? ---#

# Se filtran aquellas rutas que sufrieron retrasos en la salida del vuelo

Vuelos_filter <- filter(Vuelos2020_Org, DEP_DEL15 == 1)

Ruta_Vuelo <- Vuelos_filter %>%
  group_by(FLIGHT_ROUT)%>%
  count(FLIGHT_ROUT) 

Ruta_Vuelo_Filter <- filter(Ruta_Vuelo, n > 140)

Rutas <- c("EWR-MCO","LAX-SFO","MCO-EWR","ORD-LGA","SEA-SFO","SFO-LAX")
Retrasos <- c(159,203,155,163,160,209)

barplot(height=Retrasos, names=Rutas, 
        xlab="Rutas", 
        ylab="Cantidad de atrasos", 
        main="Atrasos según rutas", 
        ylim=c(0,215))
                            
##
