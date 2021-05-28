rm(list=ls())

library(dplyr)
library(stringr)
library(car)
library(ggplot2)
Energia1 <- read.delim("Energia1.txt") %>% 
  arrange(PAISES)

# valores para Uru en los años 1984,1991,1993,1994,1995, 1996.

pais <- str_sub(Energia1$PAISES,1,-3)
anio <- (str_sub(Energia1$PAISES,-2))
anio<-paste(19,anio,sep="" ) %>% 
  as.numeric()
max(anio)
min(anio)
unique(pais) #hay paises duplicados

for (i in 1:144) {
 pais[i]= gsub(", ","_",pais[i])
 pais[i]= gsub(" _","_",pais[i])
 pais[i]= gsub("_", " ",pais[i])
}

unique(pais) #24 paises

Energia1$Año<-anio
Energia1$paises_n<-pais

ggplot(Energia1 ,aes(Año,paises_n,fill=Prod_Ind))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_viridis_c(name="Prod IND",option ="C")+
  labs(title= ("PBI Industrial total"), x="Año", y="Países")+
  scale_x_continuous(breaks =c(1984,1991,1993,1994,1995,1996))
#  Perú es el país cuyo PBI indsutrial más aumentó en los 90. Leves aumentos en Argentina, Barbados por unos años,
#  Uruguay. EN otros casos disminuyó (Jamaica, Ecuador)

ggplot(Energia1 %>% filter(Año== 1996) ,aes(X.IMPORTA,ConsENpc,color=paises_n,size=PBIpc))+
  geom_point()+
  geom_label(aes(label=(paises_n)))+
  labs(title="% Importado en oferta energética vs Consumo final pc", subtitle = "Año 1984")


########## proyecto

library(Rcmdr)
library(FactoMineR)
library(RcmdrPlugin.FactoMineR)
library(Factoshiny)

summary(Energia1) #todas las variables son cuantitativas -> lo más útil será ACP

# Correlación 

cor(Energia1[1:24,2:15]) # Correlaciones altas: 
#Prod_Ind-X.C_IND 0.87181122; Prod_ENE-PBIpc 0.9268224 ; Prod_ENE-X.IMPORTA 0.82742916 ; 
# ConsENpc-PBIpc 0.8105287 ; ConsENpc-O_PRIpc = 0.9702329 ; ConsENpc-O_SECpc 0.9032520 ; ConsENpc-CO2pc_cons -0.8348153  ; ConsENpc-X.C_SEC 0.9360467 ; ConsENpc-C_ELECpc 0.8597183 ;
# PBIpc-Prod_Ene  0.9268224 ; PBIpc-O_SECpc 0.9591698; PBIpc- C_ELECpc 0.8218865 ;
# O_PRIpc- O_SECpc  0.87450511 ; O_PRIpc-CO2_cons -0.80239813 ; O_PRIpc-X.C_SEC  -0.92102668 ; 
# O_SECpc - X.C_SEC  0.8961128 ; O_SECpc-C_ELECpc  0.8164657 ;CO2pc - X.IMPORTA 0.81533895 ; X.C_RES-C_ELECpc -0.8940039; X.C_SEC -C_ELECpc 0.80593479
# 
