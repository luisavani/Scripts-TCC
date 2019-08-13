setwd("C:/Users/luisa/Google Drive/UFSC/TCC/PLANILHAS")
library(readxl)
dados <- read_excel("Plan_Fauna total_Wilson Kalina Isa_CONFERENCIA 2018.xlsx", sheet=6, 
                    col_names=T, na="", skip=0, col_types=NULL)
#dados <- read.table("clipboard",header=T, dec=",") 
#dados<-dados[which(dados$ESTUARIO != "ITA"),]
#dados<-dados[which(dados$ESTUARIO != "ARI"),]
dados<-dados[which(dados$ESTUARIO != "MAR"),]
#dados<-dados[which(dados$ESTUARIO != "MAS"),]
#dados<-dados[which(dados$ESTUARIO != "RTN"),]
dados<-dados[which(dados$ESTUARIO != "LLT"),]
#FAUNA3
library(sciplot)

#pdf("aaabund.pdf", width =20, height = 15)
par(mfrow=c(5,2), mar = c(3, 2, 1, 2), oma = c(1, 4, 1, 1), lwd=0.4)
#-----------------------------------------ABUND TOTAL POLUIDO--------------------------
#ARIRIU ITACORUBI MARUIM
bargraph.CI(x.factor = PERIODOc, response = ATOTAL, data=dadosfauna, 
            ylim=c(0,1500), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                 "springgreen4", "springgreen", "springgreen4", "springgreen"))
axis(side = 2, line=0, at = c(0, 500, 1000, 1500), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Abundância total", side=2 , line=2.6, cex=0.8)
legend("topleft",c("Aririu","Itacorubi","Ratones","Massiambu"),
       pch=c(15),col=c("red", "red4", "springgreen4", "springgreen"),cex = 1)

bargraph.CI(x.factor = PERIODOc, response = RIQUEZA, data=dados, 
            ylim=c(0,12), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                     "springgreen4", "springgreen", "springgreen4", "springgreen"))
axis(side = 2, line=0, at = c(0, 4, 8, 12), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Riqueza", side=2 , line=2.6, cex=0.8)

bargraph.CI(x.factor = PERIODOc, response = CAPITELLA, data=dados, 
            ylim=c(0,200), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen4", "springgreen", "springgreen4", "springgreen"))
axis(side = 2, line=0, at = c(0,100, 200), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Capitella spp.", side=2 , line=2.6, cex=0.8)

bargraph.CI(x.factor = PERIODOc, response = LAEONEREIS, data=dados, 
            ylim=c(0,150), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen4", "springgreen", "springgreen4", "springgreen"))
axis(side = 2, line=0, at = c(0,50, 100, 150), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Laeoneresi culverii", side=2 , line=2.6, cex=0.8)

bargraph.CI(x.factor = PERIODOc, response = TUBIFICINAE, data=dados, 
            ylim=c(0,70), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen4", "springgreen", "springgreen4", "springgreen"))
axis(side = 2, line=0, at = c(0, 35, 70), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Tubificinae", side=2 , line=2.6, cex=0.8)

bargraph.CI(x.factor = PERIODOc, response = STREBLOSPIO, data=dados, 
            ylim=c(0,100), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen4", "springgreen", "springgreen4", "springgreen"))
axis(side = 2, line=0, at = c(0,50, 100), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Streblospio benedicti", side=2 , line=2.6, cex=0.8)

bargraph.CI(x.factor = PERIODOc, response = KALLIAPSEUDES, data=dados, 
            ylim=c(0,1200), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen4", "springgreen", "springgreen4", "springgreen"))
axis(side = 2, line=0, at = c(0,600, 1200), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Kalliapseudes schubartii", side=2 , line=2.6, cex=0.8)

bargraph.CI(x.factor = PERIODOc, response = THOLOZODIUM, data=dados, 
            ylim=c(0,100), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen4", "springgreen", "springgreen4", "springgreen"))
axis(side = 2, line=0, at = c(0,50, 100), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Tholozodium rhombofrontalis", side=2 , line=2.6, cex=0.8)

bargraph.CI(x.factor = PERIODOc, response = HETEROMASTUS, data=dados, 
            ylim=c(0,60), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, mgp=c(3, 0.5, 0), cex.axis= 1, col = c("red", "red4", "red", "red4", 
                                                                                "springgreen4", "springgreen", "springgreen4", "springgreen"))
axis(side = 2, line=0, at = c(0, 30, 60), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Heteromastus sp.", side=2 , line=2.6, cex=0.8)
#-----------------------------------------ABUND TOTAL NÃO POLUIDO-----------------------------
#LAGOINHA MASSIAMBU RATONES
bargraph.CI(x.factor = PERIODOc, response = ATOTAL, data=dados[199:396,], 
            ylim=c(0,1300), axes = F, err.width = 0.03, lc=F, main = "", las=1,
            font.main = 1, cex.main = 1, cex.axis= 1, mgp=c(0,2.4,-0.2), 
            col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0, 200, 400, 600, 800, 1000, 1200), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
#mtext(text="Abundância total", side=2 , line=2.7, cex=1)
mtext(text = rep(c("LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------RIQUEZA POLUIDO--------------------------
#ARIRIU ITACORUBI MARUIM
bargraph.CI(x.factor = PERIODOc, response = RIQUEZA, data=dados[1:198,], 
            ylim=c(0,10), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0, 2, 4, 6, 8, 10), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Riqueza", side=2 , line=2.7, cex=1)
mtext(text = rep(c("ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------RIQUEZA NÃO POLUIDO-----------------------------
#LAGOINHA MASSIAMBU RATONES
bargraph.CI(x.factor = PERIODOc, response = RIQUEZA, data=dados[199:396,], 
            ylim=c(0,10), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2.4,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0, 2, 4, 6, 8, 10), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
#mtext(text="Riqueza", side=2 , line=2.7, cex=1)
mtext(text = rep(c("LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------CAPITELLA POLUIDO-----------------------------------
#ARIRIU ITACORUBI MARUIM
bargraph.CI(x.factor = PERIODOc, response = CAPITELLA, data=dados[1:198,], 
            ylim=c(0,200), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0,50, 100, 150, 200), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Capitella spp.", font=3, side=2 , line=2.7, cex=1)
mtext(text = rep(c("ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------CAPITELLA NÃO POLUIDO--------------------------------
#LAGOINHA MASSIAMBU RATONES
bargraph.CI(x.factor = PERIODOc, response = CAPITELLA, data=dados[199:396,], 
            ylim=c(0,200), axes = F, err.width = 0.03, lc=F, main = "", las=1,mgp=c(0,2.4,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0,50, 100, 150, 200), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
#mtext(text="Capitella spp.", side=2 , line=2.7, cex=1)
mtext(text = rep(c("LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)
#-----------------------------------------LAEONEREIS POLUIDO-----------------------------------
#ARIRIU ITACORUBI MARUIM
bargraph.CI(x.factor = PERIODOc, response = ABUNDTOTALLAEO, data=dados[1:198,],
            ylim=c(0,1100), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0,250, 500, 750, 1100), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Laeonereis culverii", font=3, side=2 , line=2.7, cex=1)
mtext(text = rep(c("ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------LAEONEREIS NÃO POLUIDO--------------------------------
#LAGOINHA MASSIAMBU RATONES
bargraph.CI(x.factor = PERIODOc, response = ABUNDTOTALLAEO, data=dados[199:396,], 
            ylim=c(0,1100), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2.4,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0,250, 500, 750, 1100), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
#mtext(text="Laeonereis culverii", side=2 , line=2.7, cex=1)
mtext(text = rep(c("LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)
#-----------------------------------------KALLIAPSEUDES POLUIDO-----------------------------------
#ARIRIU ITACORUBI MARUIM
bargraph.CI(x.factor = PERIODOc, response = KALLIAPSEUDES, data=dados[1:198,], 
            ylim=c(0,1300), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0,300, 600, 900, 1300), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Kalliapseudes schubarti", font=3, side=2 , line=2.7, cex=1)
mtext(text = rep(c("ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------KALLIAPSEUDES NÃO POLUIDO--------------------------------
#LAGOINHA MASSIAMBU RATONES
bargraph.CI(x.factor = PERIODOc, response = KALLIAPSEUDES, data=dados[199:396,], 
            ylim=c(0,1300), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0,300, 600, 900, 1300), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text = rep(c("LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------STREBLOSPIO POLUIDO-----------------------------------
#ARIRIU ITACORUBI MARUIM
bargraph.CI(x.factor = PERIODOc, response = STREBLOSPIO, data=dados[1:198,], 
            ylim=c(0,150), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0, 50, 100, 150), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Streblospio benedicti", font=3, side=2 , line=2.7, cex=1)
mtext(text = rep(c("ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------STREBLOSPIO NÃO POLUIDO--------------------------------
#LAGOINHA MASSIAMBU RATONES
bargraph.CI(x.factor = PERIODOc, response = STREBLOSPIO, data=dados[199:396,], 
            ylim=c(0,150), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0, 50, 100, 150), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text = rep(c("LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------THOLOZODIUM POLUIDO-----------------------------------
#ARIRIU ITACORUBI MARUIM
bargraph.CI(x.factor = PERIODOc, response = THOLOZODIUM, data=dados[1:198,], 
            ylim=c(0,200), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0, 50, 100, 150, 200), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Tholozodium sp.", font=3, side=2 , line=2.7, cex=1)
mtext(text = rep(c("ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------THOLOZODIUM NÃO POLUIDO--------------------------------
#LAGOINHA MASSIAMBU RATONES
bargraph.CI(x.factor = PERIODOc, response = THOLOZODIUM, data=dados[199:396,], 
            ylim=c(0,200), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0, 50, 100, 150, 200), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text = rep(c("LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)


#-----------------------------------------TUBIFICINAE POLUIDO-----------------------------------
#ARIRIU ITACORUBI MARUIM
bargraph.CI(x.factor = PERIODOc, response = TUBIFICINAE, data=dados[1:198,], 
            ylim=c(0,200), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0, 50, 100, 150, 200), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text="Tubicinae", side=2 , line=2.7, cex=1)
mtext(text = rep(c("ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR","ARI", "ITA", "MAR",
                   "ARI", "ITA", "MAR")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

#-----------------------------------------TUBIFICINAE NÃO POLUIDO--------------------------------
#LAGOINHA MASSIAMBU RATONES
bargraph.CI(x.factor = PERIODOc, response = TUBIFICINAE, data=dados[199:396,], 
            ylim=c(0,200), axes = F, err.width = 0.03, lc=F, main = "", las=1, mgp=c(0,2,-0.2),
            font.main = 1, cex.main = 1, cex.axis= 1, col = c("gray15", "gray94", "gray36"))
axis(side = 2, line=0, at = c(0, 50, 100, 150, 200), tcl = -0.25, cex.axis = 1, 
     mgp=c(3,0.6,0), las=1, labels = T)
mtext(text = rep(c("LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT","LLT", "MAS", "RAT",
                   "LLT", "MAS", "RAT")), at = c(1.5, 2.6, 3.6, 
                                                 5.6, 6.6, 7.6, 
                                                 9.6, 10.6, 11.6, 
                                                 13.6, 14.6, 15.6, 
                                                 17.6, 18.6, 19.6,
                                                 21.6, 22.6, 23.6,
                                                 25.6, 26.6, 27.6,
                                                 29.6, 30.6, 31.6,
                                                 33.6, 34.6, 35.6,
                                                 37.6, 38.6, 39.6,
                                                 41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)

dev.off()
mtext(text = rep(c("ARI", "MAR","MAS", "RTN",
                   "ARI", "MAR","MAS", "RTN",
                   "ARI", "MAR","MAS", "RTN",
                   "ARI", "MAR","MAS", "RTN",
                   "ARI", "MAR","MAS", "RTN",
                   "ARI", "MAR","MAS", "RTN")), at = c(1.5, 2.6, 3.6, 
                                                       5.6, 6.6, 7.6, 
                                                       9.6, 10.6, 11.6, 
                                                       13.6, 14.6, 15.6, 
                                                       17.6, 18.6, 19.6,
                                                       21.6, 22.6, 23.6,
                                                       25.6, 26.6, 27.6,
                                                       29.6, 30.6, 31.6,
                                                       33.6, 34.6, 35.6,
                                                       37.6, 38.6, 39.6,
                                                       41.6, 42.6, 43.6), 
      las= 2, side=1, line=0.3, cex=0.6)
