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
D_1<-5
z_critico <- qnorm(1 - (1 - 0.95) / 2)
n_1 <- ceiling(((z_critico^2)*(std^2))/(D^2))
#print(n)


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
print(std_2)
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
#print(std_3)
D_3<-2.5
z_critico <- qnorm(1 - (1 - 0.95) / 2)
n_3 <- ceiling(((z_critico^2)*(std_3^2))/(D_3^2))
print(n_3)

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
