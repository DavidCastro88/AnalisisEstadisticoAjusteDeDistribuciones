install.packages("MASS")
install.packages("fitdistrplus")
install.packages("moments")
install.packages("readxl")
require(fitdistrplus)
library(readxl)

ruta_archivo <- "C:/Users/David/Desktop/U/SimulacióndeSistemas/TrabajoSDS/Datos.xlsx"
# Leer la hoja específica del archivo XLSX


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

#Ajuste de Distribuciones 

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

#Fuente de entrada: Tiempo de servicio
datos_tiempo_servicio <- read_excel(ruta_archivo, sheet = "Tiempo_de_servicio")
# Imprimir los datos leídos
print(datos_tiempo_servicio)
#Hallamos el número de datos necesarios
muestra_1 = datos_tiempo_servicio$Tiempo_total_segundos[1:30]
std_1 <- sd(muestra_1)
mean_1 <- mean(muestra_1)
D_1<-5
z_critico <- qnorm(1 - (1 - 0.95) / 2)
n_1 <- ceiling(((z_critico^2)*(std_1^2))/(D_1^2))
#print(mean_1)


Tiempos_de_servicio <- datos_tiempo_servicio$Tiempo_total_segundos[1:n_1]
# Cálculo de estadísticos descriptivos básicos
summary(Tiempos_de_servicio)

# plotdist grafica el histograma de frecuencias de x y la distribución acumulada
win.graph(width=6,height=4,pointsize=8) 
plotdist(Tiempos_de_servicio,histo=TRUE,demp=TRUE)

win.graph(width=10,height=5,pointsize=10) 
descdist(Tiempos_de_servicio,boot=100)

#Distribuciones Gamma, Weibull, Lognormal

fit_1_lognormal<-fitdist(Tiempos_de_servicio,"lnorm")
fit_1_weibull<-fitdist(Tiempos_de_servicio,"weibull")
fit_1_gamma<- fitdist(Tiempos_de_servicio,"gamma")
#fit_1_normal<- fitdist(Tiempos_de_servicio,"norm")
#fit_1_exp<- fitdist(Tiempos_de_servicio,"exp")
#fit_1_logis<- fitdist(Tiempos_de_servicio,"logis")
#fit_1_unif<- fitdist(Tiempos_de_servicio,"unif")


summary(fit_1_lognormal)
summary(fit_1_weibull)
summary(fit_1_gamma)
#summary(fit_1_normal)
#summary(fit_1_exp)
#summary(fit_1_logis)
#summary(fit_1_unif)

win.graph(width=5,height=5,pointsize=15) 
plot(fit_1_lognormal)
win.graph(width=5,height=5,pointsize=15) 
plot(fit_1_weibull)
win.graph(width=5,height=5,pointsize=15) 
plot(fit_1_gamma)

win.graph(width=5,height=5,pointsize=10) 
par(mfrow=c(2,2))
plot.legend <- c("lognormal","weibull","gamma")
denscomp(list(fit_1_lognormal, fit_1_weibull,fit_1_gamma), legendtext = plot.legend)
cdfcomp (list(fit_1_lognormal, fit_1_weibull,fit_1_gamma), legendtext = plot.legend)
qqcomp  (list(fit_1_lognormal, fit_1_weibull,fit_1_gamma), legendtext = plot.legend)
ppcomp  (list(fit_1_lognormal, fit_1_weibull,fit_1_gamma), legendtext = plot.legend)

gofstat(list(fit_1_lognormal,fit_1_weibull,fit_1_gamma))

#Mejor funcion Lognormal 


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

#Fuente de entrada: Tiempo de pago bono

datos_tiempo_de_pago_bono <- read_excel(ruta_archivo, sheet = "Tiempo_de_pago_bono")
# Imprimir los datos leídos
print(datos_tiempo_de_pago_bono)
#Hallamos el número de datos necesarios
muestra_2 = datos_tiempo_de_pago_bono$Tiempo_total_segundos[1:30]
std_2 <- sd(muestra_2)
mean_2<-mean(muestra_2)
print(mean_2)
D_2<-2
z_critico <- qnorm(1 - (1 - 0.95) / 2)
n_2 <- ceiling(((z_critico^2)*(std_2^2))/(D_2^2))
print(n_2)

Tiempos_de_pago_bono <- datos_tiempo_de_pago_bono$Tiempo_total_segundos[1:n_2]
Tiempos_de_pago_bono
# Cálculo de estadísticos descriptivos básicos
summary(Tiempos_de_pago_bono)

# plotdist grafica el histograma de frecuencias de x y la distribución acumulada
win.graph(width=6,height=4,pointsize=8) 
plotdist(Tiempos_de_pago_bono,histo=TRUE,demp=TRUE)

win.graph(width=10,height=5,pointsize=10) 
descdist(Tiempos_de_pago_bono,boot=100)

#Distribuciones Gamma, Weibull, Lognormal

fit_2_lognormal<-fitdist(Tiempos_de_pago_bono,"lnorm")
fit_2_weibull<-fitdist(Tiempos_de_pago_bono,"weibull")
fit_2_gamma<- fitdist(Tiempos_de_pago_bono,"gamma")
#fit_2_normal<- fitdist(Tiempos_de_pago_bono,"norm")
#fit_2_exp<- fitdist(Tiempos_de_pago_bono,"exp")
#fit_2_logis<- fitdist(Tiempos_de_pago_bono,"logis")
#fit_2_unif<- fitdist(Tiempos_de_pago_bono,"unif")


summary(fit_2_lognormal)
summary(fit_2_weibull)
summary(fit_2_gamma)
#summary(fit_2_normal)
#summary(fit_2_exp)
#summary(fit_2_logis)
#summary(fit_2_unif)

win.graph(width=5,height=5,pointsize=15) 
plot(fit_2_lognormal)
win.graph(width=5,height=5,pointsize=15) 
plot(fit_2_weibull)
win.graph(width=5,height=5,pointsize=15) 
plot(fit_2_gamma)

win.graph(width=5,height=5,pointsize=10) 
par(mfrow=c(2,2))
plot.legend <- c("lognormal","weibull","gamma")
denscomp(list(fit_2_lognormal, fit_2_weibull,fit_2_gamma), legendtext = plot.legend)
cdfcomp (list(fit_2_lognormal, fit_2_weibull,fit_2_gamma), legendtext = plot.legend)
qqcomp  (list(fit_2_lognormal, fit_2_weibull,fit_2_gamma), legendtext = plot.legend)
ppcomp  (list(fit_2_lognormal, fit_2_weibull,fit_2_gamma), legendtext = plot.legend)

gofstat(list(fit_2_lognormal,fit_2_weibull,fit_2_gamma))

#Mejor funcion Lognormal 


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

#Fuente de entrada: Tiempo de pago efectivo

datos_tiempo_de_pago_efectivo <- read_excel(ruta_archivo, sheet = "Tiempo_de_pago_efectivo")
# Imprimir los datos leídos
print(datos_tiempo_de_pago_efectivo)
#Hallamos el número de datos necesarios
muestra_3 = datos_tiempo_de_pago_efectivo$Tiempo_total_segundos[1:30]
std_3 <- sd(muestra_3)
mean_3 <- mean(muestra_3)
#print(mean_3)
D_3<-2.5
z_critico <- qnorm(1 - (1 - 0.95) / 2)
n_3 <- ceiling(((z_critico^2)*(std_3^2))/(D_3^2))
#print(n_3)

Tiempos_de_pago_efectivo <- datos_tiempo_de_pago_efectivo$Tiempo_total_segundos[1:n_3]
Tiempos_de_pago_efectivo
# Cálculo de estadísticos descriptivos básicos
summary(Tiempos_de_pago_efectivo)

# plotdist grafica el histograma de frecuencias de x y la distribución acumulada
win.graph(width=6,height=4,pointsize=8) 
plotdist(Tiempos_de_pago_efectivo,histo=TRUE,demp=TRUE)

win.graph(width=10,height=5,pointsize=10) 
descdist(Tiempos_de_pago_efectivo,boot=100)

#Distribuciones Gamma, Weibull, Lognormal

fit_3_lognormal<-fitdist(Tiempos_de_pago_efectivo,"lnorm")
fit_3_weibull<-fitdist(Tiempos_de_pago_efectivo,"weibull")
fit_3_gamma<- fitdist(Tiempos_de_pago_efectivo,"gamma")
#fit_3_normal<- fitdist(Tiempos_de_pago_efectivo,"norm")
#fit_3_exp<- fitdist(Tiempos_de_pago_efectivo,"exp")
#fit_3_logis<- fitdist(Tiempos_de_pago_efectivo,"logis")
#fit_3_unif<- fitdist(Tiempos_de_pago_efectivo,"unif")


summary(fit_3_lognormal)
summary(fit_3_weibull)
summary(fit_3_gamma)
#summary(fit_3_normal)
#summary(fit_3_exp)
#summary(fit_3_logis)
#summary(fit_3_unif)

win.graph(width=5,height=5,pointsize=15) 
plot(fit_3_lognormal)
win.graph(width=5,height=5,pointsize=15) 
plot(fit_3_weibull)
win.graph(width=5,height=5,pointsize=15) 
plot(fit_3_gamma)

win.graph(width=5,height=5,pointsize=10) 
par(mfrow=c(2,2))
plot.legend <- c("lognormal","weibull","gamma")
denscomp(list(fit_3_lognormal, fit_3_weibull,fit_3_gamma), legendtext = plot.legend)
cdfcomp (list(fit_3_lognormal, fit_3_weibull,fit_3_gamma), legendtext = plot.legend)
qqcomp  (list(fit_3_lognormal, fit_3_weibull,fit_3_gamma), legendtext = plot.legend)
ppcomp  (list(fit_3_lognormal, fit_3_weibull,fit_3_gamma), legendtext = plot.legend)

gofstat(list(fit_3_lognormal,fit_3_weibull,fit_3_gamma))

#Mejor funcion Gamma


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

#Fuente de entrada: Tiempo entre llegadas

datos_tiempo_entre_llegadas <- read_excel(ruta_archivo, sheet = "Tiempo_entre_llegadas")
# Imprimir los datos leídos
print(datos_tiempo_entre_llegadas)
#Hallamos el número de datos necesarios
muestra_4 = datos_tiempo_entre_llegadas$Tiempo_total_segundos[1:30]
std_4 <- sd(muestra_4)
mean_4 <- mean(muestra_4)
print(std_4)
D_4<-2
z_critico <- qnorm(1 - (1 - 0.95) / 2)
n_4 <- ceiling(((z_critico^2)*(std_4^2))/(D_4^2))
print(n_4)

Tiempos_entre_llegadas <- datos_tiempo_entre_llegadas$Tiempo_total_segundos[1:n_4]
Tiempos_entre_llegadas
# Cálculo de estadísticos descriptivos básicos
summary(Tiempos_entre_llegadas)

# plotdist grafica el histograma de frecuencias de x y la distribución acumulada
win.graph(width=6,height=4,pointsize=8) 
plotdist(Tiempos_entre_llegadas,histo=TRUE,demp=TRUE)

win.graph(width=10,height=5,pointsize=10) 
descdist(Tiempos_entre_llegadas,boot=100)

#Distribuciones Gamma, Weibull, Lognormal

fit_4_lognormal<-fitdist(Tiempos_entre_llegadas,"lnorm")
fit_4_weibull<-fitdist(Tiempos_entre_llegadas,"weibull")
fit_4_gamma<- fitdist(Tiempos_entre_llegadas,"gamma")
#fit_4_normal<- fitdist(Tiempos_entre_llegadas,"norm")
#fit_4_exp<- fitdist(Tiempos_entre_llegadas,"exp")
#fit_4_logis<- fitdist(Tiempos_entre_llegadas,"logis")
#fit_4_unif<- fitdist(Tiempos_entre_llegadas,"unif")


summary(fit_4_lognormal)
summary(fit_4_weibull)
summary(fit_4_gamma)
#summary(fit_4_normal)
#summary(fit_4_exp)
#summary(fit_4_logis)
#summary(fit_4_unif)

win.graph(width=5,height=5,pointsize=15) 
plot(fit_4_lognormal)
win.graph(width=5,height=5,pointsize=15) 
plot(fit_4_weibull)
win.graph(width=5,height=5,pointsize=15) 
plot(fit_4_gamma)

win.graph(width=5,height=5,pointsize=10) 
par(mfrow=c(2,2))
plot.legend <- c("lognormal","weibull","gamma")
denscomp(list(fit_4_lognormal, fit_4_weibull,fit_4_gamma), legendtext = plot.legend)
cdfcomp (list(fit_4_lognormal, fit_4_weibull,fit_4_gamma), legendtext = plot.legend)
qqcomp  (list(fit_4_lognormal, fit_4_weibull,fit_4_gamma), legendtext = plot.legend)
ppcomp  (list(fit_4_lognormal, fit_4_weibull,fit_4_gamma), legendtext = plot.legend)

gofstat(list(fit_4_lognormal,fit_4_weibull,fit_4_gamma))

#Mejor funcion Lognormal

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

#Fuente de entrada: Tiemps de pago Nequi

datos_tiempo_de_pago_nequi <- read_excel(ruta_archivo, sheet = "Tiempo_de_pago_nequi")
# Imprimir los datos leídos
print(datos_tiempo_de_pago_nequi)
#Hallamos el número de datos necesarios
muestra_5 = datos_tiempo_de_pago_nequi$Tiempo_total_segundos[1:30]
std_5 <- sd(muestra_5)
mean_5 <- mean(muestra_5)
print(mean_5)
D_5<-3
z_critico <- qnorm(1 - (1 - 0.95) / 2)
n_5 <- ceiling(((z_critico^2)*(std_5^2))/(D_5^2))
print(n_5)

Tiempos_de_pago_nequi <- datos_tiempo_de_pago_nequi$Tiempo_total_segundos[1:n_5]
Tiempos_de_pago_nequi
# Cálculo de estadísticos descriptivos básicos
summary(Tiempos_de_pago_nequi)

# plotdist grafica el histograma de frecuencias de x y la distribución acumulada
win.graph(width=6,height=4,pointsize=8) 
plotdist(Tiempos_de_pago_nequi,histo=TRUE,demp=TRUE)

win.graph(width=10,height=5,pointsize=10) 
descdist(Tiempos_de_pago_nequi,boot=100)

#Distribuciones Gamma, Weibull, Lognormal

fit_5_lognormal<-fitdist(Tiempos_de_pago_nequi,"lnorm")
fit_5_weibull<-fitdist(Tiempos_de_pago_nequi,"weibull")
fit_5_gamma<- fitdist(Tiempos_de_pago_nequi,"gamma")
#fit_5_normal<- fitdist(Tiempos_de_pago_nequi,"norm")
#fit_5_exp<- fitdist(Tiempos_de_pago_nequi,"exp")
#fit_5_logis<- fitdist(Tiempos_de_pago_nequi,"logis")
#fit_5_unif<- fitdist(Tiempos_de_pago_nequi,"unif")


summary(fit_5_lognormal)
summary(fit_5_weibull)
summary(fit_5_gamma)
#summary(fit_5_normal)
#summary(fit_5_exp)
#summary(fit_5_logis)
#summary(fit_5_unif)

win.graph(width=5,height=5,pointsize=15) 
plot(fit_5_lognormal)
win.graph(width=5,height=5,pointsize=15) 
plot(fit_5_weibull)
win.graph(width=5,height=5,pointsize=15) 
plot(fit_5_gamma)

win.graph(width=5,height=5,pointsize=10) 
par(mfrow=c(2,2))
plot.legend <- c("lognormal","weibull","gamma")
denscomp(list(fit_5_lognormal, fit_5_weibull,fit_5_gamma), legendtext = plot.legend)
cdfcomp (list(fit_5_lognormal, fit_5_weibull,fit_5_gamma), legendtext = plot.legend)
qqcomp  (list(fit_5_lognormal, fit_5_weibull,fit_5_gamma), legendtext = plot.legend)
ppcomp  (list(fit_5_lognormal, fit_5_weibull,fit_5_gamma), legendtext = plot.legend)

gofstat(list(fit_5_lognormal,fit_5_weibull,fit_5_gamma))

#Mejor funcion lognormal 