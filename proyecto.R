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
unique(anio)
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
#  Perú es el país cuyo PBI industrial más aumentó en los 90. Leves aumentos en Argentina, Barbados por unos años,
#  Uruguay. EN otros casos disminuyó (Jamaica, Ecuador)

ggplot(Energia1 %>% filter(Año== 1996) ,aes(X.IMPORTA,ConsENpc,color=paises_n,size=PBIpc))+
  geom_point()+
  geom_label(aes(label=(paises_n)))+
  labs(title="% Importado en oferta energética vs Consumo final pc", subtitle = "Año 1984")

rownames(Energia1)=Energia1$PAISES
Energia_st<-scale(Energia1[,2:15])
#### VARIABLES

summary(Energia1) #todas las variables son cuantitativas -> lo más útil será ACP
by(Energia1[,2:15],Energia1[,16],summary) #no se  ven cambios importantes en Productividad, emisiones de Co2, etc.
# Correlación 

cor(Energia1[Energia1$Año==1984,2:15]) # Correlaciones altas en 1984: 
#Prod_Ene-PBIpc 0.83538542; ConsENpc-O_PRIpc 0.84860289 ;ConsENpc-C_ELEpc 0.80772690  ; O_PRIpc-C_ELECpc 0.8202470 

cor(Energia1[Energia1$Año==1996,2:15]) # Correlaciones altas en 1996: 
#Prod_Ene-PBIpc 0.810985198; ConsENpc-O_PRIpc 0.915940442  ;ConsENpc_C - X.C_IND 0.890504571  ; X-C_SEC-X.C_TRAN 0.85696498 
#
#
acp1984=PCA(Energia_st[Energia_st$Año==1984,c(4:17)])
acp1996=PCA(Energia_st[Energia_st$Año==1996,c(4:17)])


res.pca1984 = PCA(Energia_st[Energia_st$Año==1984,c(4:17)], scale.unit=TRUE, ncp=4, graph = FALSE)
resshiny1984 = PCAshiny(res.pca1984)

ploplot.PCA(res.PCA,choix='var',habillage = 'cos2',title="Gráfico perspectiva de variables")
plot.PCA(res.PCA,habillage='cos2',title="Gráfico perspectiva de países",col.ind='#ED1536')
summary(res.PCA1996)
res.pca1984$eig
ggplot(a,aes(x=seq(1:14),y=eigenvalue))+
  geom_line()+
  geom_label(aes(label=round(`percentage of variance`,2)))+
  labs(title = "Varianza explicada por cada dimensión",x="Número del valor propio", y="Valores propios")+
  scale_x_continuous(breaks =seq(1:14))
  
a<-as.data.frame(res.pca1984$eig)

fviz_eig(acp1984,ylab = "% de inercia explicada", xlab = "Valores propios", main = "Inercia explicada por los componentes")
get_eigenvalue(acp1984)
# las dos primeras dimensiones sólo aportan un 63.3% de la varianza, con 3 se explica 76.18%
## VARIABLES
# D1 correlacion alta con PBIpc(0.81) , O_SECpc(0.92), XC_RES(-0.9) , CELECpc (0.92) , CONSENpc (0.85)
# D2 correlacion alta con Prod_Ind (0.85)
# D3 correlacion alta con CO2_con (0.91), CO2pc( 0.72)
# D4 correlacion alta con X.Importa (0.95)
# 
## PAISES:
# D1 representa bien a : Argentina, El Salvador, Guatemala, Haiti, Honduras, Nicaragua, Venezuela.
# D2 : mejora la representacion de Arg, Honduras, y Venezuela; Uruguay, Brasil (aunque sigue siendo baja, 60%)
# D3: con D1 mejora representacion de Trinidad,Paraguay,Bolivia; con D4 Paraguay
# D4 con D1 mejora representación de Barbados y Cuba


res.pca1996 = PCA(Energia_st[Energia_st$Año==1996,c(4:17)], scale.unit=TRUE, ncp=4, graph = FALSE)
resshiny1996 = PCAshiny(res.pca1996)

plot.PCA(res.PCA,choix='var',habillage = 'contrib',title="Gráfico perspectiva variables")
plot.PCA(res.PCA,habillage='contrib',title="Gráfico perspectiva de países")
summary(res.pca1996)


evplot = function(ev) {  ### FUNCION UTIL PARA ELEGIR COMP CON EL CRITERIO BS
  # Broken stick model (MacArthur 1957)  
  n = length(ev)  
  bsm = data.frame(j=seq(1:n), p=0)  
  bsm$p[1] = 1/n  
  for (i in 2:n) bsm$p[i] = bsm$p[i-1] + (1/(n + 1 - i))  
  bsm$p = 100*bsm$p/n  
  # Plot eigenvalues and % of variation for each axis  
  op = par(mfrow=c(2,1),omi=c(0.1,0.3,0.1,0.1), mar=c(1, 1, 1, 1))  
  barplot(ev, main="Valores propios", col="bisque", las=2)  
  abline(h=mean(ev), col="red")  
  legend("topright", "Valor propio medio", lwd=1, col=2, bty="n")  
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE,   
          main="% Inercia", col=c("bisque",2), las=2)  
  legend("topright", c("% Valor propio", "Criterio Broken Stick"),   
         pch=15, col=c("bisque",2), bty="n")  
  par(op)  
} 
evplot()

evplot(aa$values)

hcpc1984<-HCPC(res.pca1984, graph = FALSE,nb.clust = 3)
fviz_cluster(hcpc1984, repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Caracterización de países para 1984")

hcpc1996<-HCPC(acp1996, graph = FALSE,nb.clust = 3)
fviz_cluster(hcpc1996, repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Caracterización de países para 1984")

fviz_pca_biplot(acp1984)
fviz_pca_biplot(acp1984)
