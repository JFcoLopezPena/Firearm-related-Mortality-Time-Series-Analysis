library(tidyverse)
library(glmnet)
library(haven)
library(stringr)
library(ggplot2)
library(tseries)
library(grid)
library(tibble)
library(gtable)
library(httr)
library(jsonlite)
library(rjson)
library("TTR")


suma_registros <- function(datos) {
  suma <- aggregate(datos$year, by = list(month = datos$month, intent = datos$intent), FUN = length)
  colnames(suma) <- c("Month", "Intent", "Suma")
  return(suma)
}

a<-c()
b<-c()
c<-c()
d<-c()

#Datos 2006
datos_2006 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2006.csv")
datos_limpios_2006<-suma_registros(datos_2006)
lista_causas_2006 <- split(datos_limpios_2006, datos_limpios_2006$Intent)
accidental_2006<- lista_causas_2006[["Accidental"]]
homicide_2006<-lista_causas_2006[["Homicide"]]
suicide_2006<-lista_causas_2006[["Suicide"]]
undeter_2006<-lista_causas_2006[["Undetermined"]]

a<-c(a,accidental_2006$Suma)
b<-c(b, homicide_2006$Suma)
c<-c(c,suicide_2006$Suma)

#Datos 2007
datos_2007 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2007.csv")
datos_limpios_2007<-suma_registros(datos_2007)
lista_causas_2007 <- split(datos_limpios_2007, datos_limpios_2007$Intent)
accidental_2007<- lista_causas_2007[["Accidental"]]
homicide_2007<-lista_causas_2007[["Homicide"]]
suicide_2007<-lista_causas_2007[["Suicide"]]
undeter_2007<-lista_causas_2007[["Undetermined"]]

a<-c(a,accidental_2007$Suma)
b<-c(b, homicide_2007$Suma)
c<-c(c,suicide_2007$Suma)

#Datos 2008
datos_2008 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2008.csv")
datos_limpios_2008<-suma_registros(datos_2008)
lista_causas_2008 <- split(datos_limpios_2008, datos_limpios_2008$Intent)
accidental_2008<- lista_causas_2008[["Accidental"]]
homicide_2008<-lista_causas_2008[["Homicide"]]
suicide_2008<-lista_causas_2008[["Suicide"]]
undeter_2008<-lista_causas_2008[["Undetermined"]]

a<-c(a,accidental_2008$Suma)
b<-c(b, homicide_2008$Suma)
c<-c(c,suicide_2008$Suma)

#Datos 2009
datos_2009 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2009.csv")
datos_limpios_2009<-suma_registros(datos_2009)
lista_causas_2009 <- split(datos_limpios_2009, datos_limpios_2009$Intent)
accidental_2009<- lista_causas_2009[["Accidental"]]
homicide_2009<-lista_causas_2009[["Homicide"]]
suicide_2009<-lista_causas_2009[["Suicide"]]
undeter_2009<-lista_causas_2009[["Undetermined"]]

a<-c(a,accidental_2009$Suma)
b<-c(b, homicide_2009$Suma)
c<-c(c,suicide_2009$Suma)

#Datos 2010
datos_2010 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2010.csv")
datos_limpios_2010<-suma_registros(datos_2010)
lista_causas_2010 <- split(datos_limpios_2010, datos_limpios_2010$Intent)
accidental_2010<- lista_causas_2010[["Accidental"]]
homicide_2010<-lista_causas_2010[["Homicide"]]
suicide_2010<-lista_causas_2010[["Suicide"]]
undeter_2010<-lista_causas_2010[["Undetermined"]]

a<-c(a,accidental_2010$Suma)
b<-c(b, homicide_2010$Suma)
c<-c(c,suicide_2010$Suma)

#Datos 2011
datos_2011 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2011.csv")
datos_limpios_2011<-suma_registros(datos_2011)
lista_causas_2011 <- split(datos_limpios_2011, datos_limpios_2011$Intent)
accidental_2011 <- lista_causas_2011[["Accidental"]]
homicide_2011<-lista_causas_2011[["Homicide"]]
suicide_2011<-lista_causas_2011[["Suicide"]]
undeter_2011<-lista_causas_2011[["Undetermined"]]

a<-c(a,accidental_2011$Suma)
b<-c(b, homicide_2011$Suma)
c<-c(c,suicide_2011$Suma)

#Datos 2012
datos_2012 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2012.csv")
datos_limpios_2012<-suma_registros(datos_2012)
lista_causas_2012 <- split(datos_limpios_2012, datos_limpios_2012$Intent)
accidental_2012 <- lista_causas_2012[["Accidental"]]
homicide_2012<-lista_causas_2012[["Homicide"]]
suicide_2012<-lista_causas_2012[["Suicide"]]
undeter_2012<-lista_causas_2012[["Undetermined"]]

a<-c(a,accidental_2012$Suma)
b<-c(b, homicide_2012$Suma)
c<-c(c,suicide_2012$Suma)

#Datos 2013
datos_2013 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2013.csv")
datos_limpios_2013<-suma_registros(datos_2013)
lista_causas_2013 <- split(datos_limpios_2013, datos_limpios_2013$Intent)
accidental_2013 <- lista_causas_2013[["Accidental"]]
homicide_2013<-lista_causas_2013[["Homicide"]]
suicide_2013<-lista_causas_2013[["Suicide"]]
undeter_2013<-lista_causas_2013[["Undetermined"]]

a<-c(a,accidental_2013$Suma)
b<-c(b, homicide_2013$Suma)
c<-c(c,suicide_2013$Suma)

#Datos 2014
datos_2014 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2014.csv")
datos_limpios_2014<-suma_registros(datos_2014)
lista_causas_2014<- split(datos_limpios_2014, datos_limpios_2014$Intent)
accidental_2014 <- lista_causas_2014[["Accidental"]]
homicide_2014<-lista_causas_2014[["Homicide"]]
suicide_2014<-lista_causas_2014[["Suicide"]]
undeter_2014<-lista_causas_2014[["Undetermined"]]

a<-c(a,accidental_2014$Suma)
b<-c(b, homicide_2014$Suma)
c<-c(c,suicide_2014$Suma)

#Datos 2015
datos_2015 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2015.csv")
datos_limpios_2015<-suma_registros(datos_2015)
lista_causas_2015 <- split(datos_limpios_2015, datos_limpios_2015$Intent)
accidental_2015 <- lista_causas_2015[["Accidental"]]
homicide_2015<-lista_causas_2015[["Homicide"]]
suicide_2015<-lista_causas_2015[["Suicide"]]
undeter_2015<-lista_causas_2015[["Undetermined"]]

a<-c(a,accidental_2015$Suma)
b<-c(b, homicide_2015$Suma)
c<-c(c,suicide_2015$Suma)

#Datos 2016
datos_2016 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2016.csv")
datos_limpios_2016<-suma_registros(datos_2016)
lista_causas_2016 <- split(datos_limpios_2016, datos_limpios_2016$Intent)
accidental_2016 <- lista_causas_2016[["Accidental"]]
homicide_2016<-lista_causas_2016[["Homicide"]]
suicide_2016<-lista_causas_2016[["Suicide"]]
undeter_2016<-lista_causas_2016[["Undetermined"]]

a<-c(a,accidental_2016$Suma)
b<-c(b, homicide_2016$Suma)
c<-c(c,suicide_2016$Suma)

#Datos 2017
datos_2017 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2017.csv")
datos_limpios_2017<-suma_registros(datos_2017)
lista_causas_2017 <- split(datos_limpios_2017, datos_limpios_2017$Intent)
accidental_2017 <- lista_causas_2017[["Accidental"]]
homicide_2017<-lista_causas_2017[["Homicide"]]
suicide_2017<-lista_causas_2017[["Suicide"]]
undeter_2017<-lista_causas_2017[["Undetermined"]]

a<-c(a,accidental_2017$Suma)
b<-c(b, homicide_2017$Suma)
c<-c(c,suicide_2017$Suma)
d<-c(d,undeter_2017$Suma)

#Datos 2018
datos_2018 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2018.csv")
datos_limpios_2018<-suma_registros(datos_2018)
lista_causas_2018 <- split(datos_limpios_2018, datos_limpios_2018$Intent)
accidental_2018 <- lista_causas_2018[["Accidental"]]
homicide_2018<-lista_causas_2018[["Homicide"]]
suicide_2018<-lista_causas_2018[["Suicide"]]
undeter_2018<-lista_causas_2018[["Undetermined"]]

a<-c(a,accidental_2018$Suma)
b<-c(b, homicide_2018$Suma)
c<-c(c,suicide_2018$Suma)
d<-c(d,undeter_2018$Suma)

#DATOS 2019
datos_2019 <- read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2019.csv")
datos_limpios_2019<-suma_registros(datos_2019)
lista_causas_2019 <- split(datos_limpios_2019, datos_limpios_2019$Intent)
accidental_2019 <- lista_causas_2019[["Accidental"]]
homicide_2019<-lista_causas_2019[["Homicide"]]
suicide_2019<-lista_causas_2019[["Suicide"]]
undeter_2019<-lista_causas_2019[["Undetermined"]]

a<-c(a,accidental_2019$Suma)
b<-c(b, homicide_2019$Suma)
c<-c(c,suicide_2019$Suma)
d<-c(d,undeter_2019$Suma)

#DATOS 2020
datos_2020<-read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\Proba\\guns_2020.csv")
datos_limpios_2020<-suma_registros(datos_2020)
lista_causas_2020 <- split(datos_limpios_2020, datos_limpios_2020$Intent)
  accidental_2020 <- lista_causas_2020[["Accidental"]]
  homicide_2020<-lista_causas_2020[["Homicide"]]
  suicide_2020<-lista_causas_2020[["Suicide"]]
  undeter_2020<-lista_causas_2020[["Undetermined"]]

  
  a<-c(a,accidental_2020$Suma)
  b<-c(b,homicide_2020$Suma)
  c<-c(c,suicide_2020$Suma)
  d<-c(d,undeter_2020$Suma)

  shapiro.test(accidental_2006$Suma)
  shapiro.test(accidental_2007$Suma)
  shapiro.test(accidental_2008$Suma)
  shapiro.test(accidental_2009$Suma)
  shapiro.test(accidental_2010$Suma)
  shapiro.test(accidental_2011$Suma)
  shapiro.test(accidental_2012$Suma)
  shapiro.test(accidental_2013$Suma)
  shapiro.test(accidental_2014$Suma)
  shapiro.test(accidental_2015$Suma)
  shapiro.test(accidental_2016$Suma)
  shapiro.test(accidental_2017$Suma)
  shapiro.test(accidental_2018$Suma)
  shapiro.test(accidental_2019$Suma)
  shapiro.test(accidental_2020$Suma)
  
print(mean(a))
  st_a<-ts(a, start=2006, frequency = 12)
    plot.ts(st_a)
      spectrum(st_a)
  log_st_a <- log(st_a)
    plot.ts(log_st_a)
  sma_a<-SMA(st_a, frecuency=180)
    sma_a
  a_forecast<-HoltWinters(st_a, beta=FALSE, gamma=FALSE)
    plot(a_forecast)
    #La suma de los squared-errors
    a_forecast$SSE
    #Los pronósticos hechos por HoltWinters() están almacenados en 
    a_forecast$fitted
    
    aaa<-forecast:::forecast.HoltWinters(a_forecast, h=180)
          plot(aaa, main= 'Muertes accidentales')

    cor_a<-acf(st_a)
      cor_a
      
      serie_descompuesta_a <- decompose(st_a)
      plot(serie_descompuesta_a)
      
      # Ajustar el modelo ARMA con órdenes separados de AR y MA
      modelo_arma_a <- arima(st_a, order = c(1, 0, 1), method="CSS")
      modelo_arma_a
      plot(modelo_arma_a)
      # Obtener los coeficientes de AR del modelo ARMA
      coeficientes_ar_a <- modelo_arma_a$coef[1:1]
      # Obtener los coeficientes de MA del modelo ARMA
      coeficientes_ma_a <- modelo_arma_a$coef[(1+1):(1+1)]
      # Obtener los residuos del modelo ARMA
      residuos_a <- modelo_arma_a$residuals
      # Obtener diagnósticos del modelo ARMA
      diagnosticos_a <- modelo_arma_a$arma
      shapiro.test(a)
      


shapiro.test(homicide_2006$Suma)
shapiro.test(homicide_2007$Suma)
shapiro.test(homicide_2008$Suma)
shapiro.test(homicide_2009$Suma)
shapiro.test(homicide_2010$Suma)
shapiro.test(homicide_2011$Suma)
shapiro.test(homicide_2012$Suma)
shapiro.test(homicide_2013$Suma)
shapiro.test(homicide_2014$Suma)
shapiro.test(homicide_2015$Suma)
shapiro.test(homicide_2016$Suma)
shapiro.test(homicide_2017$Suma)
shapiro.test(homicide_2018$Suma)
shapiro.test(homicide_2019$Suma)
shapiro.test(homicide_2020$Suma)

  print(mean(b))
   st_b<-ts(b, start=2006, frequency = 12)
     plot.ts(st_b)
      log_st_b <- log(st_b)
      plot.ts(log_st_b)
        sma_b<-SMA(st_b, frecuency=15)
        sma_b
          b_forecast<-HoltWinters(st_b, beta=FALSE, gamma=FALSE)
          bbb<-forecast:::forecast.HoltWinters(b_forecast, h=180)
            plot(bbb, main='Homicidios')
            cor_b<-acf(st_b)
            cor_b
            serie_descompuesta_b <- decompose(st_b)
            plot(serie_descompuesta_b)
            # Ajustar el modelo ARMA con órdenes separados de AR y MA
            modelo_arma_b <- arima(st_b, order = c(1, 0, 1), method="CSS")
            modelo_arma_b
            plot(modelo_arma_b)
            # Obtener los coeficientes de AR del modelo ARMA
            coeficientes_ar_b <- modelo_arma_b$coef[1:1]
            # Obtener los coeficientes de MA del modelo ARMA
            coeficientes_ma_b <- modelo_arma_b$coef[(1+1):(1+1)]
            # Obtener los residuos del modelo ARMA
            residuos_b <- modelo_arma_b$residuals
            # Obtener diagnósticos del modelo ARMA
            diagnosticos_b <- modelo_arma_b$arma
            
            shapiro.test(b)
            
shapiro.test(suicide_2006$Suma)
shapiro.test(suicide_2007$Suma)
shapiro.test(suicide_2008$Suma)
shapiro.test(suicide_2009$Suma)
shapiro.test(suicide_2010$Suma)
shapiro.test(suicide_2011$Suma)
shapiro.test(suicide_2012$Suma)
shapiro.test(suicide_2013$Suma)
shapiro.test(suicide_2014$Suma)
shapiro.test(suicide_2015$Suma)
shapiro.test(suicide_2016$Suma)
shapiro.test(suicide_2017$Suma)
shapiro.test(suicide_2018$Suma)
shapiro.test(suicide_2019$Suma)
shapiro.test(suicide_2020$Suma)        

print(mean(c))
  st_c<-ts(c, start=2006, frequency = 12)
  plot.ts(st_c)
  log_st_c <- log(st_c)
    plot.ts(log_st_c)
      sma_c<-SMA(st_c, frecuency=15)   
      c_forecast<-HoltWinters(st_c, beta=FALSE, gamma=FALSE)
      ccc<-forecast:::forecast.HoltWinters(c_forecast, h=180)
      plot(ccc, main='Suicidios')
      cor_c<-acf(st_c)
      cor_c
      serie_descompuesta_c <- decompose(st_c)
      plot(serie_descompuesta_c)
      # Ajustar el modelo ARMA con órdenes separados de AR y MA
      modelo_arma_c <- arima(st_c, order = c(1, 0, 1), method="CSS")
      modelo_arma_c
      plot(modelo_arma_c)
      # Obtener los coeficientes de AR del modelo ARMA
      coeficientes_ar_c <- modelo_arma_c$coef[1:1]
      # Obtener los coeficientes de MA del modelo ARMA
      coeficientes_ma_c <- modelo_arma_c$coef[(1+1):(1+1)]
      # Obtener los residuos del modelo ARMA
      residuos_c <- modelo_arma_c$residuals
      # Obtener diagnósticos del modelo ARMA
      diagnosticos_c <- modelo_arma_c$arma
      shapiro.test(c)
   
      
    
shapiro.test(undeter_2006$Suma)  
shapiro.test(undeter_2007$Suma)  
shapiro.test(undeter_2008$Suma)  
shapiro.test(undeter_2009$Suma)  
shapiro.test(undeter_2010$Suma)  
shapiro.test(undeter_2011$Suma)  
shapiro.test(undeter_2012$Suma)  
shapiro.test(undeter_2013$Suma)  
shapiro.test(undeter_2014$Suma)  
shapiro.test(undeter_2015$Suma)  
shapiro.test(undeter_2016$Suma)  
shapiro.test(undeter_2017$Suma)  
shapiro.test(undeter_2018$Suma)  
shapiro.test(undeter_2019$Suma)  
shapiro.test(undeter_2020$Suma)     

print(mean(d))
  st_d<-ts(d, start=2006, frequency = 12)
  plot.ts(st_d)
    log_st_d <- log(st_d)
    plot.ts(log_st_d)
      sma_d<-SMA(st_d, frecuency=15)  
      d_forecast<-HoltWinters(st_d, beta=FALSE, gamma=FALSE)
      ddd<-forecast:::forecast.HoltWinters(d_forecast, h=180)
      plot(ddd, main='Causa indeterminada')
      cor_d<-acf(st_d)
      cor_d
      serie_descompuesta_d <- decompose(st_d)
      plot(serie_descompuesta_d)
      # Ajustar el modelo ARMA con órdenes separados de AR y MA
      modelo_arma_d <- arima(st_d, order = c(1, 0, 1), method="CSS")
      modelo_arma_d
      plot(modelo_arma_d)
      # Obtener los coeficientes de AR del modelo ARMA
      coeficientes_ar_d <- modelo_arma_d$coef[1:1]
      # Obtener los coeficientes de MA del modelo ARMA
      coeficientes_ma_d <- modelo_arma_d$coef[(1+1):(1+1)]
      # Obtener los residuos del modelo ARMA
      residuos_d <- modelo_arma_d$residuals
      # Obtener diagnósticos del modelo ARMA
      diagnosticos_d <- modelo_arma_d$arma
      
      shapiro.test(d)
      

    
  
      
      
      
      


