#rm(list=ls()) #limpar memória rm= remove memorie
setwd("C:/Users/luisa/Google Drive/UFSC/TCC/PLANILHAS")
library(readxl)
dadobrutoS <- read_excel("Plan_Fauna total_Wilson Kalina Isa_CONFERENCIA 2018.xlsx", 
                         sheet=6, col_names=T, na="", skip=0, col_types=NULL)
pca <- read.table("clipboard",header=T, dec=",")
prectemp <- read.table("clipboard",header=T, dec=",")
abio <- read.table("clipboard",header=T, dec=",") 

dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "ITA"),]
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "ARI"),]
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "MAR"),]
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "MAS"),]
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "RTN"),]
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "LLT"),]
dadobrutoSa<- sqrt(sqrt(dadobrutoS[,9:15]))
dadobrutoSa<- sqrt(sqrt(dadobrutoS[,10:40]))

abio<-abio[which(abio$ESTUARIO != "ITA"),]
abio<-abio[which(abio$ESTUARIO != "ARI"),]
abio<-abio[which(abio$ESTUARIO != "MAR"),]
abio<-abio[which(abio$ESTUARIO != "MAS"),]
abio<-abio[which(abio$ESTUARIO != "RTN"),]
abio<-abio[which(abio$ESTUARIO != "LLT"),]

library(vegan)
library(GAD)
library(labdsv)
library(MASS)
library(pairwiseAdonis)

options(max.print=100000)
## PCA
pca <- pca(prectemp, dim =2) #ABIO.PCA #PRECTEMP.PCA
summary(pca)

# CAP
dadobruto.cap <- capscale(dadobrutoSa ~ PRECTEMP.PCA, pca, distance = "euclidean")
summary(dadobruto.cap)

scl <- 3 ## scaling = 4
colvec <- c("red", "red4", "springgreen4", "springgreen")                          

plot(dadobruto.cap, type = "n", scaling = 3, xlim=c(-1.5,1), ylim=c(-1,1.5))
with(abio, points(dadobruto.cap, display = "sites", col = colvec[ESTUARIO],
                      scaling = scl, cex= 1.5,pch = 21, bg = colvec[ESTUARIO]))
legend("bottomleft",c("Itacorubi", "Aririu", "Ratones", "Massiambu"),
       pch=c(19),col=c("red4", "red", "springgreen", "springgreen4"),cex = 1)
text(dadobruto.cap,labels=dadobrutoS$ESTUARIO, scaling = 3, cex=0.5, col=c("black"))
summary(STRESS<-stressplot(dadobruto.cap))

#PARTIÇÃO DE VARIÂNCIA
var5 <- varpart(dadobrutoSa, prectemp, abio)  #euclidean
var4 <- varpart(vegdist(dadobrutoSa), prectemp, abio)  #por bray curtis

plot(var5)
plot(var4) 

#PERMANOVA
CO <- as.fixed(dadobrutoS$CONDICAO)
ES <- as.random(dadobrutoS$ESTUARIO)
PE <- as.fixed(dadobrutoS$PERIODOa)
ME <- as.fixed(dadobrutoS$MES)

ad <- adonis(dadobrutoSa ~ CO + ES%in%CO + PE + CO*PE,
             dadobrutoS[,1:5], permutations = 999,
             method = "bray")

#POST HOC
pairwise.adonis(x=dadobrutoSa, sim.function="vegdist", sim.method = "bray",
                p.adjust.m = "bonferroni", factors= PE, perm = 999)
