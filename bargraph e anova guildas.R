setwd("C:/Users/luisa/Google Drive/UFSC/TCC/PLANILHAS")
library(readxl)
dados <- read_excel("Plan_Fauna total_Wilson Kalina Isa_CONFERENCIA 2018.xlsx", sheet=7, 
                    col_names=T, na="", skip=0, col_types=NULL)

dados <- read.table("clipboard",header=T, dec=",") 

#TIRAR ESTUÁRIOS QUE NÃO VOU USAR
dados<-dados[which(dados$ESTUARIO != "ITA"),]
dados<-dados[which(dados$ESTUARIO != "ARI"),]
dados<-dados[which(dados$ESTUARIO != "MAR"),]
dados<-dados[which(dados$ESTUARIO != "MAS"),]
dados<-dados[which(dados$ESTUARIO != "RTN"),]
dados<-dados[which(dados$ESTUARIO != "LLT"),]

#Pacotes
library(sciplot)
library(GAD)

CO <- as.fixed(dados$CONDICAO)
PE <- as.fixed(dados$PERIODOa)

summary(aov<-aov(Predator ~ CO + PE+ PE*CO, dados))
TukeyHSD(aov)

head(dados)
par(mfrow=c(5,2), mar = c(3, 2, 1, 2), oma = c(1, 4, 1, 1), lwd=0.4)
#-----------------------------------------ABUND TOTAL POLUIDO--------------------------
#ARIRIU ITACORUBI MARUIM
bargraph.CI(x.factor = PERIODOc, response = Burrower, data=dados, 
            ylim=c(0,320), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen", "springgreen4", "springgreen", "springgreen4"))
axis(side = 2, line=0, at = c(0, 80, 160, 240, 320), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Burrower", side=2 , line=2.6, cex=0.8)


bargraph.CI(x.factor = PERIODOc, response = Surfacedep, data=dados, 
            ylim=c(0,40), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen", "springgreen4", "springgreen", "springgreen4"))
axis(side = 2, line=0, at = c(0, 20, 40), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Surface-dep ", side=2 , line=2.6, cex=0.8)

bargraph.CI(x.factor = PERIODOc, response = Suspensivore, data=dados, 
            ylim=c(0,1200), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen", "springgreen4", "springgreen", "springgreen4"))
axis(side = 2, line=0, at = c(0,600, 1200), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Suspensivore", side=2 , line=2.6, cex=0.8)

bargraph.CI(x.factor = PERIODOc, response = Predator, data=dados, 
            ylim=c(0,5), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen", "springgreen4", "springgreen", "springgreen4"))
axis(side = 2, line=0, at = c(0,5), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="deposit feeders", side=2 , line=2.6, cex=0.8)

