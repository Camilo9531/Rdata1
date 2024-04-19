#TRABAJO NR.2 CAMILO AGURTO

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

#Pregunta de investigacion: ¿De qué manera la satisfaccion con la democracia en Chile
# se ve afectada por el nivel de desconfianza hacía las instituciones políticas?
#(gobierno, partidos políticos, poder judicial y congreso)

#Variable dependiente: 
# Satisfaccion con la democracia en chile: c01

#Variables independientes:  
#Confianza en el gobierno: c05_01, 
#confianza en los partidos politicos: c05_02,
#confianza en el poder judicial: c05_05
#confianza en el congreso nacional: c05_07

#Seleccion de variables a utilizar

investigacion <- select(ELSOC,c01,c05_01,c05_02,c05_05,c05_07)

#Valores descriptivos

descr(investigacion)
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

#recodificacion variables: pasamos de satisfaccion a insatisfaccion, tambien de 
# de confianza a desconfianza, para ello revertimos los valores de todas las variables

investigacion$c01 <- recode(investigacion$c01, "1=0;2=1;3=2;4=3;5=4")
investigacion$c05_01 <- recode(investigacion$c05_01,"1=4;2=3;3=2;4=1;5=0")
investigacion$c05_02 <- recode(investigacion$c05_02,"1=4;2=3;3=2;4=1;5=0")
investigacion$c05_05 <- recode(investigacion$c05_05,"1=4;2=3;3=2;4=1;5=0")
investigacion$c05_07 <- recode(investigacion$c05_07,"1=4;2=3;3=2;4=1;5=0")


#Renombrar variables

investigacion <- investigacion %>% rename("satif_demo"=c01,
                                          "desconf_gob"=c05_01,
                                          "desconf_ppolit"=c05_02,
                                          "desconf_pjudicial"=c05_05,
                                          "desconf_cong"=c05_07)
investigacion$satif_demo <- set_label(x=investigacion$satif_demo,label = "satisfaccion:democracia")
get_label(investigacion$satif_demo)
investigacion$desconf_gob <- set_label(x=investigacion$desconf_gob,label = "desconfianza:Gobierno")
get_label(investigacion$desconf_gob)
investigacion$desconf_ppolit <- set_label(x=investigacion$desconf_ppolit,label = "desconfianza:partidospoliticos")
get_label(investigacion$desconf_ppolit)
investigacion$desconf_pjudicial <- set_label(x=investigacion$desconf_pjudicial,label = "desconfianza:poderjudicial")
get_label(investigacion$desconf_pjudicial)
investigacion$desconf_cong <- set_label(x=investigacion$desconf_cong,label = "desconfianza:Congreso")
get_label(investigacion$desconf_cong)

#arreglo de recodificacion

investigacion$satif_demo <- set_labels(investigacion$satif_demo,
                                 labels=c( "nada satisfecho"=0,
                                           "poco satisfecho"=1,
                                           "algo satisfecho"=2,
                                           "bastante satisfecho"=3,
                                           "muy satisfecho"=4))
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

frq(investigacion$satif_demo)
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

Satisfaccion_democracia <- sjPlot::plot_stackfrq(dplyr::select(investigacion,satif_demo),
                                title = "satisfaccion con la democracia") +
  theme(legend.position="bottom")
Satisfaccion_democracia

#Descriptivo de las variables
summarytools::dfSummary(investigacion, plain.ascii = FALSE)
view(dfSummary(investigacion, headings=FALSE))

descr(investigacion)
describe(investigacion)
