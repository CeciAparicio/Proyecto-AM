rm(list=ls())
########## proyecto

library(Rcmdr)
library(FactoMineR)
library(RcmdrPlugin.FactoMineR)
library(Factoshiny)
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

rownames(Energia1)=Energia1$PAISES

#### VARIABLES

summary(Energia1) #todas las variables son cuantitativas -> lo más útil será ACP

# Correlación 

cor(Energia1[Energia1$Año==1984,2:15]) # Correlaciones altas en 1984: 
#Prod_Ene-PBIpc 0.83538542; ConsENpc-O_PRIpc 0.84860289 ;ConsENpc-C_ELEpc 0.80772690  ; O_PRIpc-C_ELECpc 0.8202470 

cor(Energia1[Energia1$Año==1996,2:15]) # Correlaciones altas en 1996: 
#Prod_Ene-PBIpc 0.810985198; ConsENpc-O_PRIpc 0.915940442  ;ConsENpc_C - X.C_IND 0.890504571  ; X-C_SEC-X.C_TRAN 0.85696498 
#
acp1984=PCA(Energia1[Energia1$Año==1984,c(2:15)])
acp1996=PCA(Energia1[Energia1$Año==1996,c(2:15)])



library(Factoshiny)
res.pca1984 = PCA(Energia1[Energia1$Año==1984,c(2:15)], scale.unit=TRUE, ncp=5, graph = FALSE)
resshiny1984 = PCAshiny(res.pca1984)

plot.PCA(res.PCA,choix='var',habillage = 'contrib',title="Gráfico perspectiva de variables")
plot.PCA(res.PCA,habillage='contrib',title="Gráfico perspectiva de países",col.ind='#ED1536')
summary(res.PCA1996) # las dos primeras dimensiones sólo aportan un 63.3% de la varianza, con 3 se explica 76.18%
# ConENpc es explicada en 82.5%, PIBpc en 81.2%, O_SECpc en 92.4% , X.C_RES en en -89.6% porla primera dimension.


res.pca1996 = PCA(Energia1[Energia1$Año==1996,c(2:15)], scale.unit=TRUE, ncp=5, graph = FALSE)
resshiny1996 = PCAshiny(res.pca1996)

plot.PCA(res.PCA,choix='var',habillage = 'contrib',title="Gráfico perspectiva variables")
plot.PCA(res.PCA,habillage='contrib',title="Gráfico perspectiva de países")
summary(res.pca1996)
