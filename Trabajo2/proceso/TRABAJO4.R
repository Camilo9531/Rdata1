#TRABAJO NR.4 CAMILO AGURTO

#Valores sin notacion cientifica
options(scipen=999)

#Borrar memoria de trabajo
rm(list=ls())


#Establecer directorio de trabajo
setwd("C:/Users/Camilo Agurto/OneDrive/Documentos/GitHub/Rdata1/Trabajo2")

#cargar paquetes
library(summarytools)
library(sjlabelled)
library(readr)
library(dplyr)
library(haven)
library(sjPlot)
library(sjmisc)
library(tidyverse)
library(ggplot2)
library(psych)
library(stargazer)
library(car)
#Cargar base de datos 
ELSOC <- read_dta("C:/Users/Camilo Agurto/OneDrive/Documentos/GitHub/Rdata1/Trabajo2/input/ELSOC_W05_v1.0_Stata13.dta")

#Explorar base de datos
names(ELSOC)
dim(ELSOC)
view(ELSOC)
#Pregunta de investigacion: ¿De qué manera la desconfianza en las instituciones políticas
# influye en la insatisfacción que sienten los chilenos con la democracia?

#Variable dependiente: 
# Satisfaccion con la democracia en chile: c01

#Variables independientes:  
#Confianza en el gobierno: c05_01, 
#confianza en los partidos politicos: c05_02,
#confianza en el poder judicial: c05_05
#confianza en el congreso nacional: c05_07

#Variables de control: Region.

#Seleccion de variables a utilizar

investigacion <- select(ELSOC,region_cod,c01,c05_01,c05_02,c05_05,c05_07)

#Valores descriptivos

frq(investigacion$region_cod)
frq(investigacion$c01)
investigacion$c01 <- recode(investigacion$c01,"c(-999,-888,-777,-666)=NA")
frq(investigacion$c05_01)
investigacion$c05_01 <- recode(investigacion$c05_01,"c(-999,-888,-777,-666)=NA")
frq(investigacion$c05_02)
investigacion$c05_02 <- recode(investigacion$c05_02,"c(-999,-888,-777,-666)=NA")
frq(investigacion$c05_05)
investigacion$c05_05 <- recode(investigacion$c05_05,"c(-999,-888,-777,-666)=NA")
frq(investigacion$c05_07)
investigacion$c05_07 <- recode(investigacion$c05_07,"c(-999,-888,-777,-666)=NA")

#Borrar datos nulos

investigacion <-na.omit(investigacion)
describe(investigacion)
#recodificacion variables: pasamos de satisfaccion a insatisfaccion, tambien de 
# de confianza a desconfianza, para ello revertimos los valores de todas las variables

investigacion$c01 <- recode(investigacion$c01, "1=4;2=3;3=2;4=1;5=0")
investigacion$c05_01 <- recode(investigacion$c05_01,"1=4;2=3;3=2;4=1;5=0")
investigacion$c05_02 <- recode(investigacion$c05_02,"1=4;2=3;3=2;4=1;5=0")
investigacion$c05_05 <- recode(investigacion$c05_05,"1=4;2=3;3=2;4=1;5=0")
investigacion$c05_07 <- recode(investigacion$c05_07,"1=4;2=3;3=2;4=1;5=0")

#recodificacion a variable dummy region: region metropolitana 1, otras regiones 0 (variable metropo)

investigacion <- investigacion %>%
  mutate(metropo = case_when(
    region_cod %in% c(13) ~ 1,
    TRUE ~ 0
  ))

#Renombrar variables

investigacion <- investigacion %>% rename("insatif_demo"=c01,
                                          "desconf_gob"=c05_01,
                                          "desconf_ppolit"=c05_02,
                                          "desconf_pjudicial"=c05_05,
                                          "desconf_cong"=c05_07,
                                          "region"=region_cod)
investigacion$insatif_demo <- set_label(x=investigacion$insatif_demo,label = "insatisfaccion:democracia")
get_label(investigacion$insatif_demo)
investigacion$desconf_gob <- set_label(x=investigacion$desconf_gob,label = "desconfianza:Gobierno")
get_label(investigacion$desconf_gob)
investigacion$desconf_ppolit <- set_label(x=investigacion$desconf_ppolit,label = "desconfianza:partidospoliticos")
get_label(investigacion$desconf_ppolit)
investigacion$desconf_pjudicial <- set_label(x=investigacion$desconf_pjudicial,label = "desconfianza:poderjudicial")
get_label(investigacion$desconf_pjudicial)
investigacion$desconf_cong <- set_label(x=investigacion$desconf_cong,label = "desconfianza:Congreso")
get_label(investigacion$desconf_cong)


#arreglo de recodificacion

investigacion$insatif_demo <- set_labels(investigacion$insatif_demo,
                                 labels=c( "nada insatisfecho"=0,
                                           "poco insatisfecho"=1,
                                           "algo insatisfecho"=2,
                                           "bastante insatisfecho"=3,
                                           "muy insatisfecho"=4))
investigacion$desconf_gob <- set_labels(investigacion$desconf_gob,
                                 labels=c( "nada de desconfianza"=0,
                                           "poca desconfianza"=1,
                                           "Algo de deconfianza"=2,
                                           "bastante desconfianza"=3,
                                           "mucha desconfianza"=4))
investigacion$desconf_ppolit <- set_labels(investigacion$desconf_ppolit,
                                     labels=c( "nada de desconfianza"=0,
                                               "poca desconfianza"=1,
                                               "Algo de deconfianza"=2,
                                               "bastante desconfianza"=3,
                                               "mucha desconfianza"=4))

investigacion$desconf_pjudicial<- set_labels(investigacion$desconf_pjudicial,
                                        labels=c( "nada de desconfianza"=0,
                                                  "poca desconfianza"=1,
                                                  "Algo de deconfianza"=2,
                                                  "bastante desconfianza"=3,
                                                  "mucha desconfianza"=4))


investigacion$desconf_cong <- set_labels(investigacion$desconf_cong,
                                     labels=c( "nada de desconfianza"=0,
                                               "poca desconfianza"=1,
                                               "Algo de deconfianza"=2,
                                               "bastante desconfianza"=3,
                                               "mucha desconfianza"=4))


#Creamos variable de confianza institucional
investigacion$desconf_inst <- (investigacion$desconf_gob+investigacion$desconf_ppolit+investigacion$desconf_pjudicial+investigacion$desconf_cong)
summary(investigacion$desconf_inst)
frq(investigacion$desconf_inst)
investigacion$desconf_inst <- set_label(x=investigacion$desconf_inst,label = "desconfianza en instituciones")


#revision

frq(investigacion$insatif_demo)
frq(investigacion$desconf_gob)
frq(investigacion$desconf_ppolit)
frq(investigacion$desconf_pjudicial)
frq(investigacion$desconf_cong)

#grafico desconfianza en instituciones

investigacion %>% ggplot(aes(x = desconf_inst)) + 
  geom_bar(fill = "black")+
  labs(title = "Desconfianza en instituciones",
       x = "Desconfianza en instituciones",
       y = "Frecuencia")


grafico2 <- sjPlot::plot_stackfrq(dplyr::select(investigacion,desconf_gob,
                                              desconf_pjudicial,
                                              desconf_ppolit,
                                              desconf_cong),
                                title = "desconfianza en instituciones políticas") +
  theme(legend.position="bottom")
grafico2

#grafico satisfaccion con la democracia

insatisfaccion_democracia <- sjPlot::plot_stackfrq(dplyr::select(investigacion,insatif_demo),
                                title = "insatisfaccion con la democracia") +
  theme(legend.position="bottom")
insatisfaccion_democracia

#Descriptivo de las variables
summarytools::dfSummary(investigacion, plain.ascii = FALSE)
view(dfSummary(investigacion, headings=FALSE))

describe(investigacion)

#Correlación de Pearson

cor(investigacion, use="complete.obs")
cor(investigacion[,c("insatif_demo","desconf_inst")], use="complete.obs")
cor(investigacion[,c("insatif_demo","desconf_pjudicial")], use="complete.obs")
cor(investigacion[,c("insatif_demo","desconf_gob")], use="complete.obs")
cor(investigacion[,c("insatif_demo","desconf_ppolit")], use="complete.obs")
cor(investigacion[,c("insatif_demo","desconf_cong")], use="complete.obs")
tab_corr(investigacion, 
         triangle = "lower")
test <- cor.test(investigacion$insatif_demo, investigacion$desconf_inst, conf.level = 0.99)
print(test)

# valor p  < 0.00000000000000022, se rechaza hipotesis nula y se acepta que hay una correlacion
#significativa entre las variables desconfianza institucional e insatisfaccion con la democracia

# Modelo de regresion

modelo<-lm(insatif_demo ~ desconf_inst, data = investigacion)
grafico <- ggplot(investigacion, aes(x = desconf_inst, y = insatif_demo)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "green") + 
  labs(title = "Gráfico de Regresión Lineal",
       x = "desconfianza institucional",
       y = "insatisfaccion con la democracia")
print(grafico)
summary(modelo)

modelo2 <- lm(insatif_demo ~ desconf_inst * metropo, data = investigacion)
summary(modelo2)
tab_model(modelo2,show.se = TRUE, show.ci = FALSE)


