# ===============================================
#               Análisis VAR de las series tasa de interés nominal, inflación 
#                             y tasa de desempleo
# ===============================================

#install.packages('stargazer')
library(stargazer)
set.seed(5056)
library(readxl)
library(tseries)
library(forecast)
library(readr)
library(fUnitRoots)
library("gdata")
library("rugarch")
library("vars")
library("dynlm")
library(ggplot2)
library("seastests")
#installed.packages('Quandl')
library('Quandl')
library(xts)
library(mFilter)
library(vars)
library(urca)



#Cargar base de datos

setwd('d:/Desktop/CICLO 7/Macroeconometria/Trabajo')

desempleo <- read_excel("desempleo.xlsx")
consumonominal <- read_excel("consumo.xlsx")
deflactorconsumo <- read_excel("deflactor_consumo.xlsx")
inversionesnominal <- read_excel("inversion.xlsx")
deflactorinversion <- read_excel("deflactor_inversion.xlsx")
poblacion <-read_excel("poblacion.xlsx")

#Expresar consumo e inversi?n en variables reales:
consumopercapita <-(consumonominal[2]*(100/deflactorconsumo[2]))/poblacion[2]
inversionpercapita <-(inversionesnominal[2]*(100/deflactorinversion[2]))/poblacion[2]

#Expresar en ts
E <- ts(100-desempleo[2], 
        start = c(2000, 1), 
        end = c(2021, 4), 
        frequency = 4) 
C <- ts(consumopercapita, 
        start = c(2000, 1), 
        end = c(2021, 4), 
        frequency = 4) 
I <- ts(inversionpercapita, 
        start = c(2000, 1), 
        end = c(2021, 4), 
        frequency = 4) 

graphics.off()
par(mfrow=c(3,1))
plot.ts(E,ylab = "%",cex.main=1.5,main="Tasa Empleo",lwd=2,font.main=11,col='blue')
plot.ts(C,ylab = "Miles coronas noruegas",cex.main=1.5,main="Consumo privado real per capita",lwd=2,font.main=11,col='gray')
plot.ts(I,ylab = "Miles coronas noruegas",cex.main=1.5,main="Inversion privada real per capita",lwd=2,font.main=11,col='black')

#####-(B) Tratamiento de la data para an?lisis VAR #####

#Primero vemos tienen tendencia estoc?stica, es decir, si cuentan con ra?z unitaria
#Por an?lisis gr?fico podemos deducir que Empleo cuenta con constante (pues no inicia en 0) pero no con tendencia estoc?stica
# Consumo e Inversi?n cuentan con constante y con tendencia creciente
# Especificamos esto en el test ADF
adfTest(E,lags=1,type="c") #pvalue>0.05 -> no se rechaza H0 -> Hay Ra?z unitaria
adfTest(C,lags=1,type="ct") #pvalue>0.05 -> no se rechaza H0 -> Hay Ra?z unitaria
adfTest(I,lags=1,type="ct") #pvalue>0.05 -> no se rechaza H0 -> Hay Ra?z unitaria
#Aplicamos primeras diferencias para quitar tendencia:
dE <- diff(E)
dC <- diff(C)
dI <- diff(I)
#Graficamos:
graphics.off()
par(mfrow=c(3,1))
plot.ts(dE,ylab = "??",cex.main=1.5,main="Primera diferencia Tasa empleo",lwd=2,font.main=11,col='blue')
plot.ts(dC,ylab = "??",cex.main=1.5,main="Primera diferencia Consumo privado real per capita",lwd=2,font.main=11,col='gray')
plot.ts(dI,ylab = "??",cex.main=1.5,main="Primera diferencia Inversion privada real per capita",lwd=2,font.main=11,col='black')
#Series parecen estacionarias y con media cero, comprobamos con ADF test:
adfTest(dE,lags=1,type="nc") #pvalue<0.05 -> se rechaza H0 -> No hay Ra?z unitaria
adfTest(dC,lags=1,type="nc") #pvalue<0.05 -> se rechaza H0 -> No hay Ra?z unitaria
adfTest(dI,lags=1,type="nc") #pvalue<0.05 -> se rechaza H0 -> No hay Ra?z unitaria

#Ahora analizamos si series son estacionales
#Puede pensarse que, sobre todo el consumo, se da en mayor medida en una ?poca del a?o, cuarto trimestre por navidad por ejemplo
#O que, por como es el mercado laboral en Noruega, hay mayor empleo en un trimestre en espec?fico
#Igualmente aplicamos el test de Ollech-Webel para comprobar estacionalidad en las 3 series.
a<-combined_test(dE,freq=4)
combined_test(dC,freq=4)
combined_test(dI,freq=4)
isSeasonal(E,test="combined",freq=4)
#Por ?ltimo vemos si series tienen raiz unitaria estacional, mediante el test OCSB (Orsborn,Chui,Smith y Birnchenhall)
ocsb.test(E, lag.method = c("fixed","AIC", "BIC", "AICc"), maxlag = 4)
ocsb.test(C, lag.method = c("fixed", "AIC", "BIC", "AICc"), maxlag = 4)
ocsb.test(I, lag.method = c("fixed", "AIC", "BIC", "AICc"), maxlag = 4)
#Tampoco hay

#Por ende, serie  ya est? lista a ser trabajada, aplicando primeras diferencias, las series:
#No cuentan con tendencia estoc?stica
#Tampoco cuentan con estacionalidad
#No cuentan con raiz unitaria estacional entre trimestres

#####-(C) Estimaci?n del modelo VAR reducido #####

#Observamos el n?mero ?ptimo de rezagos usando el comando VAR select
lags_optimos <- VARselect(data.frame(dE,dC,dI),type="none",lag.max=20) #Debido a que nuestras series son d, especificamos como tipo none
lags_optimos$selection
#Nos basamos en HQ y SC y seleccionamos un rezago en nuestro modelo VAR

#Lo estimamos manualmente mediante OLS:
VAR_EQ1 <- dynlm(dE ~ L(dE, 1) + L(dC, 1) + L(dI,1), 
                 start = c(2000, 1), 
                 end = c(2021, 4))
VAR_EQ2 <- dynlm(dC ~ L(dE, 1) + L(dC, 1) + L(dI,1), 
                 start = c(2000, 1), 
                 end = c(2021, 4))
VAR_EQ3 <- dynlm(dI ~ L(dE, 1) + L(dC, 1) + L(dI,1), 
                 start = c(2000, 1), 
                 end = c(2021, 4))
#Cambiamos nombres:
names(VAR_EQ1$coefficients) <- c("Intercepto","dE_t-1","dC_t-1","dI_t-1")
names(VAR_EQ2$coefficients) <- c("Intercepto","dE_t-1","dC_t-1","dI_t-1")
names(VAR_EQ3$coefficients) <- c("Intercepto","dE_t-1","dC_t-1","dI_t-1")
#Ver significancia de los coeficientes //matriz varcovar se calcula de forma sandwich
coeftest(VAR_EQ1, vcov. = sandwich)
coeftest(VAR_EQ2, vcov. = sandwich)
coeftest(VAR_EQ3, vcov. = sandwich)
#Vemos que el ?nico coeficiente significativo al 5% es el rezago de la Inversi?n para la propia Inversi?n

#Vemos si hay ra?z unitaria en el modelo var, para ver estabilidad del modelo
VAR_data <- window(ts.union(dE, dC, dI), start = c(2020, 1), end = c(2021, 4))
modelo <- VAR(y = VAR_data, p = 1)
roots(modelo) #Las ra?ces inversas son menores a 1, por ende, no hay ra?ces unitarias
estabilidad<-stability(modelo,type="OLS-CUSUM")
plot(estabilidad$stability$dE,lwd=1.5,col="2") #Comprobamos estabilidad
#Ahora vemos an?lisis de los residuos: Si estos est?n correlacionados, puede que el modelo est?
#mal especificado y, por ende, los residuos pueden tener incluido efectos de los rezagos sobre
#la variable dependiente

# An?lisis gr?fico
par(mfrow=c(3,1))
plot(VAR_EQ1$residuals,main="Residuos para dE",ylab="valor",cex.main=1.5,lwd=0.5,font.main=11,col='blue')
plot(VAR_EQ2$residuals,main="Residuos para dC",ylab="valor",cex.main=1.5,lwd=0.5,font.main=11,col='red')
plot(VAR_EQ3$residuals,main="Residuos para dI",ylab="valor",cex.main=1.5,lwd=0.5,font.main=11,col='gray') #Parecen ser estacionarios
#Parecen ser estacionarios y con media cero, aunque varianza parece no ser constante->posible estructura arch-garch

#Par?ntesis:
#Comprobamos brevemente esto mediante un an?lisis gr?fico
par(mfrow=c(3,1))
plot(VAR_EQ1$residuals^2,main="Residuos^2 para dE",ylab="valor",cex.main=1.5,lwd=0.5,font.main=11,col='blue')
plot(VAR_EQ2$residuals^2,main="Residuos^2 para dC",ylab="valor",cex.main=1.5,lwd=0.5,font.main=11,col='red')
plot(VAR_EQ3$residuals^2,main="Residuos^2 para dI",ylab="valor",cex.main=1.5,lwd=0.5,font.main=11,col='gray') #Parecen ser estacionarios
#Claramente tienen estructuras ARCH-GARCH dE y dI; si no fuera por 2020, dC pareciera no tenerla
ARCH<-arch.test(modelo) #Test multivariado ARCH, no nos permite realizarlo


#Gr?ficamos FAS y FAP para comprobar intuici?n del an?lisis gr?fico:
par(mfrow=c(1,2))
fas1=acf(VAR_EQ1$residuals,las=1,lag.max=20,tck=.02,xlab="lags",
              ylab="FAS",main="") 
fap1=pacf(VAR_EQ1$residuals,las=1,lag.max=20,tck=.02,xlab="lags",
                ylab="FAP",main="")
title("FAS y FAP para dE", line = -1, outer = TRUE, font.main=11)

par(mfrow=c(1,2))
fas1=acf(VAR_EQ2$residuals,las=1,lag.max=20,tck=.02,xlab="lags",
         ylab="FAS",main="") 
fap1=pacf(VAR_EQ2$residuals,las=1,lag.max=20,tck=.02,xlab="lags",
          ylab="FAP",main="")
title("FAS y FAP para dC", line = -1, outer = TRUE, font.main=11)

par(mfrow=c(1,2))
fas1=acf(VAR_EQ3$residuals,las=1,lag.max=20,tck=.02,xlab="lags",
         ylab="FAS",main="") 
fap1=pacf(VAR_EQ3$residuals,las=1,lag.max=20,tck=.02,xlab="lags",
          ylab="FAP",main="")
title("FAS y FAP para dI", line = -1, outer = TRUE, font.main=11)

#Confirmamos an?lisis gr?fico, por ?ltimos test de Ljung-Box y Box-Pierce

Box.test(VAR_EQ1$residuals,lag=10,type="Ljung-Box")
Box.test(VAR_EQ1$residuals,lag=10,type="Box-Pierce")

Box.test(VAR_EQ2$residuals,lag=10,type="Ljung-Box")
Box.test(VAR_EQ2$residuals,lag=10,type="Box-Pierce")

Box.test(VAR_EQ3$residuals,lag=10,type="Ljung-Box")
Box.test(VAR_EQ3$residuals,lag=10,type="Box-Pierce")

#No se puede rechazar la nula, i.e. residuos no est?n correlacionados serialmente
#Conclusi?n, si bien hay sospechas de estructuras ARCH, los residuos lineales si est?n
#incorrelacionados serialmente, por ende, esto hace indicar que nuestro VAR reducido
#est? bien especificado.

#Test de normalidad
normalidad<-normality.test(modelo,multivariate.only = TRUE) #pvalue>0.05, distribuidos normalmente
normalidad$jb.mul$JB

#Para finalizar an?lisis del VAR(1),veremos la causalidad a la granger de nuestras variables
#Ponemos a competir dos modelos: Uno donde la variable dependiente solo depende de su rezago y otro donde tmb depende del rezafo de la otra variable independiente
grangertest(dE ~ dC,order=1,data=VAR_data) # No causalidad a la Granger
grangertest(dE ~ dI,order=1,data=VAR_data) # No causalidad a la Granger 
grangertest(dC ~ dE,order=1,data=VAR_data) # No causalidad a la Granger 
grangertest(dC ~ dI,order=1,data=VAR_data) # No causalidad a la Granger 
grangertest(dI ~ dE,order=1,data=VAR_data) # No causalidad a la Granger
grangertest(dI ~ dC,order=1,data=VAR_data) # No causalidad a la Granger
#podemos ver tambi?n causalidad conjunta:
GrangerE<- causality(modelo,cause="dE")
GrangerC<- causality(modelo,cause="dC")
GrangerI<- causality(modelo,cause="dI")
GrangerE
GrangerC
GrangerI #Estos test confirman los resultados previos
#Si bien modelo VAR(1) parece ser bueno para ajustarse a los datos, variables no parecen ser buenas para predecir




#####-(D) Estimaci?n del modelo VAR estructural #####

#Restricciones en base a un modelo de ciclos reales e hip?tesis de ingreso permanente: 
#tenemos restricciones de sims donde orden de exogeneidad = E,C e I.
amat <- diag(3)
amat[2,1]<-NA
amat[3,1]<-NA
amat[3,2]<-NA
amat
sv<-cbind(dE,dC,dI)
colnames(sc)<-cbind("dif.empleo","dif.consumo","dif.inversion")
modelosvar<-VAR(sv,p=1,type="const")
SVAR1 <- SVAR(x = modelosvar, estmethod = c("direct"), Amat = amat, Bmat = NULL, 
                  hessian = TRUE,method="BFGS")  
#Vemos nuestra matriz A estimada:
SVAR1$A #Efecto positivo contempor?neo de empleo en consumo (como era esperado) y en inversi?n
#Tmb. efecto positivo contempor?neo de consumo en inversi?n (ingreso permanente detr?s de esto)

#Hallamos IR
SVAR_ir1<-irf(SVAR1,impulse="dE", response="dE")
SVAR_ir2<-irf(SVAR1,impulse="dE", response="dC")
SVAR_ir3<-irf(SVAR1,impulse="dE", response="dI")
SVAR_ir4<-irf(SVAR1,impulse="dC", response="dE")
SVAR_ir5<-irf(SVAR1,impulse="dC", response="dC")
SVAR_ir6<-irf(SVAR1,impulse="dC", response="dI")
SVAR_ir7<-irf(SVAR1,impulse="dI", response="dE")
SVAR_ir8<-irf(SVAR1,impulse="dI", response="dC")
SVAR_ir9<-irf(SVAR1,impulse="dI", response="dI")

#Gr?ficamos
#install.packages('usethis')
library(usethis)
library(ggplot2)
library(dplyr)
#install.packages("patchwork")
library(patchwork)
library(devtools)
source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R")
library(devtools)

single_varirf <- extract_varirf(SVAR_ir1)
single_varirf_grouped <- extract_varirf(SVAR_ir1, SVAR_ir2, SVAR_ir3)
irf_all <- irf(SVAR1, impulse = "dE", n.ahead = 40, ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.9, runs = 50)
multiple_varirf <- extract_varirf(irf_all)
head(multiple_varirf)
de_de <- single_varirf_grouped %>% 
  ggplot(aes(x=period, y=irf_de_de, ymin=lower_de_de, ymax=upper_de_de)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=0.2) +
  geom_line() +
  theme_light() +
  ggtitle("Impulso dE, Respuesta dE")+
  ylab("??")+
  xlab("Trimestres") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
de_dc <- single_varirf_grouped %>% 
  ggplot(aes(x=period, y=irf_de_dc, ymin=lower_de_dc, ymax=upper_de_dc)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=0.2) +
  geom_line() +
  theme_light() +
  ggtitle("Impulso dE, Respuesta dC")+
  ylab("??")+
  xlab("Trimestres") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
de_di <- single_varirf_grouped %>% 
  ggplot(aes(x=period, y=irf_de_di, ymin=lower_de_di, ymax=upper_de_di)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=0.2) +
  geom_line() +
  theme_light() +
  ggtitle("Impulso dE, Respuesta dI")+
  ylab("??")+
  xlab("Trimestres") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
de_de
de_dc
de_di

single_varirf_2 <- extract_varirf(SVAR_ir4)
single_varirf_grouped_2 <- extract_varirf(SVAR_ir4, SVAR_ir5, SVAR_ir6)
irf_all_2 <- irf(SVAR1, impulse = "dC", n.ahead = 40, ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.9, runs = 50)
multiple_varirf_2 <- extract_varirf(irf_all_2)
head(multiple_varirf_2)
dc_de <- single_varirf_grouped_2 %>% 
  ggplot(aes(x=period, y=irf_dc_de, ymin=lower_dc_de, ymax=upper_dc_de)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=0.2) +
  geom_line() +
  theme_light() +
  ggtitle("Impulso dC, Respuesta dE")+
  ylab("??")+
  xlab("Trimestres") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
dc_dc <- single_varirf_grouped_2 %>% 
  ggplot(aes(x=period, y=irf_dc_dc, ymin=lower_dc_dc, ymax=upper_dc_dc)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=0.2) +
  geom_line() +
  theme_light() +
  ggtitle("Impulso dC, Respuesta dC")+
  ylab("??")+
  xlab("Trimestres") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
dc_di <- single_varirf_grouped_2 %>% 
  ggplot(aes(x=period, y=irf_dc_di, ymin=lower_dc_di, ymax=upper_dc_di)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=0.2) +
  geom_line() +
  theme_light() +
  ggtitle("Impulso dC, Respuesta dI")+
  ylab("??")+
  xlab("Trimestres") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
dc_de
dc_dc
dc_di

single_varirf_3 <- extract_varirf(SVAR_ir7)
single_varirf_grouped_3 <- extract_varirf(SVAR_ir7, SVAR_ir8, SVAR_ir9)
irf_all_3 <- irf(SVAR1, impulse = "dI", n.ahead = 40, ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.9, runs = 50)
multiple_varirf_3 <- extract_varirf(irf_all_3)
head(multiple_varirf_3)
di_de <- single_varirf_grouped_3 %>% 
  ggplot(aes(x=period, y=irf_di_de, ymin=lower_di_de, ymax=upper_di_de)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=0.2) +
  geom_line() +
  theme_light() +
  ggtitle("Impulso dI, Respuesta dE")+
  ylab("??")+
  xlab("Trimestres") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
di_dc <- single_varirf_grouped_3 %>% 
  ggplot(aes(x=period, y=irf_di_dc, ymin=lower_di_dc, ymax=upper_di_dc)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=0.2) +
  geom_line() +
  theme_light() +
  ggtitle("Impulso dI, Respuesta dC")+
  ylab("??")+
  xlab("Trimestres") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
di_di <- single_varirf_grouped_3 %>% 
  ggplot(aes(x=period, y=irf_di_di, ymin=lower_di_di, ymax=upper_di_di)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=0.2) +
  geom_line() +
  theme_light() +
  ggtitle("Impulso dI, Respuesta dI")+
  ylab("??")+
  xlab("Trimestres") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
di_de
di_dc
di_di

#Ahora ver descomposici?n de la varianza:
vardesc<-fevd(modelo,n.ahead=5)
vardesc
stargazer(vardesc$dE,type="text",header=T,title="Descomposici?n para dE")
stargazer(vardesc$dC,type="text",header=T,title="Descomposici?n para dC")
stargazer(vardesc$dI,type="text",header=T,title="Descomposici?n para dI")

win.graph(width=15,height=9)
plot(fevd(data.frame(dE,dC,dI), n.ahead = 10 ))





#####-(E) An?lisis de cointegraci?n #####

#Sospechas de cointegraci?n entre el consumo per capita y la inversi?n per capita
graphics.off()
# ?Cointegran?
plot.ts(C,main="Relaci?n LP entre\n Consumo e Inversi?n",type="l", ylab="Coronas noruegas", xlab="Tiempo", cex.main=1.2, font.main=11, font.sub=11, font.lab=10,font.axis=10,col="blue",lwd=1,ylim=c(20,80))
lines(I,col="red")
legend("topleft", legend = c('Consumo','Inversi?n'), fill=c("blue","red"),text.font=10,inset=0,cex=0.75)
#Si bien se ve que ambas tienen una tendencia creciente, no es clara la presencia de cointegraci?n
#Gr?fica m?s clara para ver presencia de cointegraci?n:
plot(merge(as.zoo(C), as.zoo(I)), 
     plot.type = "single", 
     lty = c(2, 1),
     lwd = 2,
     xlab = "Fecha",
     ylab = "Coronas noruegas",
     ylim = c(20, 80),
     main = "Relaci?n C-I")

Diferencia <- as.ts(C-I)
# add the term spread series
lines(as.zoo(Diferencia), 
      col = "steelblue",
      lwd = 2,
      xlab = "Fecha",
      ylab = "Coronas noruegas",
      main = "Diferencia")

# shade the term spread
polygon(c(time(C), rev(time(C))), 
        c(I, rev(C)),
        col = alpha("steelblue", alpha = 0.3),
        border = NA)
# add horizontal line add 0
abline(25, 0)

# add a legend
legend("topleft", 
       legend = c("C", "I", "Diferencia"),
       col = c("black", "black", "steelblue"),
       lwd = c(2, 2, 2),
       lty = c(2, 1, 1),cex=0.5)
#Forma de la diferencia no parece ser estacionaria, parece ser I(1)
adfTest(Diferencia, lags=1, type = c("c")) #Ojo si asumimos Relaci?n 1-1 NO HAY COINTEGRACI?N

#Test de Engle-Granch
#Pero no sabemos relaci?n-->Debemos estimarla:
Equ1 <- dynlm(C ~ I)
plot(residuals(Equ1),ylab="Residuos C-I",ylim=c(-10,40))
lines(Diferencia,col="3") #similitud entre residuos y diferencia, probablemente que el ADF d? resultados similares a los residuos
legend("topleft", legend = c('Residuos','Diferencia simple'), fill=c("black","3"),text.font=12,inset=0,cex=0.6)
adfTest(residuals(Equ1), lags=1, type = c("nc")) #No hay cointegraci?n->Viendo estad?stico de Philips
#Por simple prueba de robustez, realizamos el test para C-E e I-E
Equ2 <- dynlm(E~C)
plot(residuals(Equ2),main="Residuos E-C",ylab="Valor residuos",cex.main=1.5,lwd=2,font.main=11,col='gray')
abline(0, 0)
adfTest(residuals(Equ2), lags=1, type = c("nc")) #No hay cointegraci?n->Viendo estad?stico de Philips porque DF dice que s? habr?a
Equ3 <- dynlm(E~I)
plot(residuals(Equ3),main="Residuos E-I",ylab="Valor residuos",cex.main=1.5,lwd=2,font.main=11,col='blue')
abline(0, 0)
adfTest(residuals(Equ3), lags=1, type = c("nc")) #No hay cointegraci?n->Viendo estad?stico de Philips porque DF dice que s? habr?a

#Test de Johansenn

#Primero para C-I
jotest_CI=ca.jo(data.frame(C,I), type="trace", K=2, ecdet="none", spec="longrun") #K=rezagos de la primeras diferencias del corrector de errores
summary(jotest_CI) #H0:r=0. no se puede rechazar, por ende no hay un vector de cointegraci?n, i.e. la funci?n MV del VECM con r=0 no es menor que con r>0

# Para C-I-E
jotest_CIE=ca.jo(data.frame(C,I,E), type="trace", K=3, ecdet="none", spec="longrun")
summary(jotest_CIE)#Tampoco se rechaza la nula, i.e. no hay ning?n vector de cointegraci?n.
?roots
