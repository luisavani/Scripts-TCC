setwd("C:/Users/luisa/Google Drive/UFSC/TCC/PLANILHAS")
library(readxl)
dadobrutoS <- read_excel("Plan_Fauna total_Wilson Kalina Isa_CONFERENCIA 2018.xlsx", 
                        sheet=6, col_names=T, na="", skip=0, col_types=NULL)
#dadobruto <- read.table("clipboard",header=T, dec=",") 
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "MAR"),]
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "LLT"),]
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "ITA"),]
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "ARI"),]
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "MAS"),]
dadobrutoS<-dadobrutoS[which(dadobrutoS$ESTUARIO != "RTN"),]

#dadobrutoSa<- log10(dadobrutoS[,7:37]+1)
dadobrutoSa<- sqrt(sqrt(dadobrutoS[,9:15]))
dadobrutoSa<- sqrt(sqrt(dadobrutoS[,10:40]))
dadobrutoSa<- sqrt(dadobrutoS[,9:39])
dadobrutoSa<- sqrt(dadobrutoS[,9:15])

library(vegan)

#dadobrutoS$zero<-rowSums(dadobrutoS)
#dadobrutoS<-dadobrutoS[which(dadobruto$zero != 0),]

#dadobrutoS.dis <- vegdist(dadobrutoS[,6:15], "bray")
mds.dadobrutoS <- metaMDS(dadobrutoSa, k=2,autotransform=F)
##------------------------------------------------------
plot(mds.dadobrutoS,type="n", main="")
points(mds.dadobrutoS$points[which(dadobrutoS$ESTUARIO == "RTN"),], cex=1.5, pch=16, col=c("springgreen4"))#RATONES
points(mds.dadobrutoS$points[which(dadobrutoS$ESTUARIO == "MAS"),], cex=1.5, pch=16, col=c("springgreen"))#MASSIAMBU
points(mds.dadobrutoS$points[which(dadobrutoS$ESTUARIO == "LLT"),], cex=1.5, pch=16, col=c("palegreen"))#LAGOINHA DO LESTE

points(mds.dadobrutoS$points[which(dadobrutoS$ESTUARIO == "MAR"),], cex=1.5, pch=16, col=c("red4"))#MARUIM
points(mds.dadobrutoS$points[which(dadobrutoS$ESTUARIO == "ITA"),], cex=1.5, pch=16, col=c("red4"))#ITACORUBI
points(mds.dadobrutoS$points[which(dadobrutoS$ESTUARIO == "ARI"),], cex=1.5, pch=16, col=c("red"))#ARIRIU

text(mds.dadobrutoS,labels=dadobrutoS$PERIODOa, cex=0.8, col=c("black"))

points(mds.dadobrutoS$points[c(34:39,58:66),], cex=1.5, pch=15, col=c("red4"))#itacorubi quente
points(mds.dadobrutoS$points[40:57,], cex=1.5, pch=0, col=c("red4"))#itacorubi frio
points(mds.dadobrutoS$points[c(1:6,25:33),], cex=1.5, pch=19, col=c("red"))#aririu quente
points(mds.dadobrutoS$points[7:24,], cex=1.5, pch=1, col=c("red"))#aririu frio

points(mds.dadobrutoS$points[c(100:105,124:132),], cex=2, pch=18, col=c("springgreen"))#Ratones quente
points(mds.dadobrutoS$points[106:123,], cex=1.5, pch=5, col=c("springgreen"))#ratones frio
points(mds.dadobrutoS$points[c(67:72,91:99),], cex=1.5, pch=17, col=c("springgreen4"))#massiambu quente
points(mds.dadobrutoS$points[73:90,], cex=1.5, pch=2, col=c("springgreen4"))#MASSIAMBU frio


legend("bottomright",c("Itacorubi", "Aririu", "Ratones", "Massiambu"),
       pch=c(15,19,18,17),col=c("red4", "red", "springgreen", "springgreen4"),cex = 1)

legend("bottomright",c("Aririu", "Itacorubi"),
       pch=c(17,15),col=c("red", "red4"),cex = 1)

legend("bottomright",c("Ratones", "Massiambu"),
       pch=c(15,17),col=c("springgreen4", "springgreen"),cex = 1)

legend("topright", c("Primavera/Verão", "Outono/Inverno"),
       pch=c(0,15),col=c("black"),cex = 1)

plot(mds.dadobrutoS,type="n", main="")
#---------------------- DADOBRUTO-C.NC-MDS #######################
points(mds.dadobrutoS$points[which(dadobrutoS$CONDICAO == "NC"),], cex=2, pch=19, col=c("blue"))#NÃO URBANO
points(mds.dadobrutoS$points[which(dadobrutoS$CONDICAO == "C"),], cex=2, pch=19, col=c("red"))#URBANO

plot(mds.dadobrutoS,type="n", main="")
points(mds.dadobrutoS$points[which(dadobrutoS$PERIODOa == "OI"),], cex=2, pch=19, col=c("blue"))#NÃO URBANO
points(mds.dadobrutoS$points[which(dadobrutoS$PERIODOa == "PV"),], cex=2, pch=19, col=c("red"))#URBANO
