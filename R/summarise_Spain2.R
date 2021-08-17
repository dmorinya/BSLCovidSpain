library(matrixStats)
library(gdata)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)

cases <- read.xls("Data/cases.xls")
colnames(cases) <- c("Date", "CCAA2", "cases")
cases$CCAA <- NA
cases$CCAA[cases$CCAA2=="AN"] <- "Andalucía"
cases$CCAA[cases$CCAA2=="AR"] <- "Aragón"
cases$CCAA[cases$CCAA2=="AS"] <- "Principado de Asturias"
cases$CCAA[cases$CCAA2=="CB"] <- "Cantabria"
cases$CCAA[cases$CCAA2=="CE"] <- "Ceuta"
cases$CCAA[cases$CCAA2=="CL"] <- "Castilla y León"
cases$CCAA[cases$CCAA2=="CM"] <- "Castilla - La Mancha"
cases$CCAA[cases$CCAA2=="CN"] <- "Canarias"
cases$CCAA[cases$CCAA2=="CT"] <- "Cataluña"
cases$CCAA[cases$CCAA2=="EX"] <- "Extremadura"
cases$CCAA[cases$CCAA2=="GA"] <- "Galicia"
cases$CCAA[cases$CCAA2=="IB"] <- "Islas Baleares"
cases$CCAA[cases$CCAA2=="MC"] <- "Región de Murcia"
cases$CCAA[cases$CCAA2=="MD"] <- "Madrid"
cases$CCAA[cases$CCAA2=="ML"] <- "Melilla"
cases$CCAA[cases$CCAA2=="NC"] <- "Comunidad Foral de Navarra"
cases$CCAA[cases$CCAA2=="PV"] <- "País Vasco"
cases$CCAA[cases$CCAA2=="RI"] <- "La Rioja"
cases$CCAA[cases$CCAA2=="VC"] <- "Comunidad Valenciana"
cases$cod_ine[cases$CCAA2=="AN"] <- 1
cases$cod_ine[cases$CCAA2=="AR"] <- 2
cases$cod_ine[cases$CCAA2=="AS"] <- 3
cases$cod_ine[cases$CCAA2=="CB"] <- 6
cases$cod_ine[cases$CCAA2=="CE"] <- 18
cases$cod_ine[cases$CCAA2=="CL"] <- 7
cases$cod_ine[cases$CCAA2=="CM"] <- 8
cases$cod_ine[cases$CCAA2=="CN"] <- 5
cases$cod_ine[cases$CCAA2=="CT"] <- 9
cases$cod_ine[cases$CCAA2=="EX"] <- 11
cases$cod_ine[cases$CCAA2=="GA"] <- 12
cases$cod_ine[cases$CCAA2=="IB"] <- 4
cases$cod_ine[cases$CCAA2=="MC"] <- 14
cases$cod_ine[cases$CCAA2=="MD"] <- 13
cases$cod_ine[cases$CCAA2=="ML"] <- 19
cases$cod_ine[cases$CCAA2=="NC"] <- 15
cases$cod_ine[cases$CCAA2=="PV"] <- 16
cases$cod_ine[cases$CCAA2=="RI"] <- 17
cases$cod_ine[cases$CCAA2=="VC"] <- 10
cases$CCAA2 <- NULL

cases <- cases[order(cases$Date, cases$CCAA),]
cases$Date <- as.Date(cases$Date, format="%Y-%m-%d")
cases <- cases[cases$Date < "2020-12-15", ]

#### Group by week
cases$Week <- week(cases$Date)

cases2 <- cases %>%
  group_by(CCAA, Week) %>%
  summarise(cases2=sum(cases))
cases <- cases2
pob <- read.xls("Data/poblacio.xls", encoding="latin1")
cases <- merge(cases, pob, by=c("CCAA"))
cases$incid <- cases$cases2/cases$Pob*100000
cases <- cases[cases$Week>7, ]
cases <- cases[cases$CCAA!="Ceuta" & cases$CCAA!="Melilla", ]
cases <- cases %>% group_by(Week, CCAA) %>% summarise(med=sum(cases2))

AN <- read.table("Results/Andalucía_incidenceDEF.csv", header=T, sep=",")/100000*8414240
q2.5 <- colQuantiles(as.matrix(AN), probs=c(0.025))
med <- colMedians(as.matrix(AN))
q97.5 <- colQuantiles(as.matrix(AN), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Andalucía"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Andalucía"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_AN <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Andalucía") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

AR <- read.table("Results/Aragón_incidenceDEF.csv", header=T, sep=",")/100000*1319291
q2.5 <- colQuantiles(as.matrix(AR), probs=c(0.025))
med <- colMedians(as.matrix(AR))
q97.5 <- colQuantiles(as.matrix(AR), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Aragón"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Aragón"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_AR <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Aragón") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

CN <- read.table("Results/Canarias_incidenceDEF.csv", header=T, sep=",")/100000*2153389
q2.5 <- colQuantiles(as.matrix(CN), probs=c(0.025))
med <- colMedians(as.matrix(CN))
q97.5 <- colQuantiles(as.matrix(CN), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Canarias"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Canarias"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_CN <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Canarias") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

CB <- read.table("Results/Cantabria_incidenceDEF.csv", header=T, sep=",")/100000*581078
q2.5 <- colQuantiles(as.matrix(CB), probs=c(0.025))
med <- colMedians(as.matrix(CB))
q97.5 <- colQuantiles(as.matrix(CB), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Cantabria"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Cantabria"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_CB <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Cantabria") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

CM <- read.table("Results/Castilla - La Mancha_incidenceDEF.csv", header=T, sep=",")/100000*2032863
q2.5 <- colQuantiles(as.matrix(CM), probs=c(0.025))
med <- colMedians(as.matrix(CM))
q97.5 <- colQuantiles(as.matrix(CM), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Castilla - La Mancha"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Castilla - La Mancha"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_CM <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Castilla - La Mancha") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

CL <- read.table("Results/Castilla y León_incidenceDEF.csv", header=T, sep=",")/100000*2399548
q2.5 <- colQuantiles(as.matrix(CL), probs=c(0.025))
med <- colMedians(as.matrix(CL))
q97.5 <- colQuantiles(as.matrix(CL), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Castilla - La Mancha"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Castilla y León"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_CL <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Castilla y León") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

CT <- read.table("Results/Cataluña_incidenceDEF.csv", header=T, sep=",")/100000*7675217
q2.5 <- colQuantiles(as.matrix(CT), probs=c(0.025))
med <- colMedians(as.matrix(CT))
q97.5 <- colQuantiles(as.matrix(CT), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Cataluña"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Cataluña"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_CT <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Cataluña") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

NC <- read.table("Results/Comunidad Foral de Navarra_incidenceDEF.csv", header=T, sep=",")/100000*654214
q2.5 <- colQuantiles(as.matrix(NC), probs=c(0.025))
med <- colMedians(as.matrix(NC))
q97.5 <- colQuantiles(as.matrix(NC), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Comunidad Foral de Navarra"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Comunidad Foral de Navarra"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_NC <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Comunidad Foral de Navarra") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

VC <- read.table("Results/Comunidad Valenciana_incidenceDEF.csv", header=T, sep=",")/100000*5003769
q2.5 <- colQuantiles(as.matrix(VC), probs=c(0.025))
med <- colMedians(as.matrix(VC))
q97.5 <- colQuantiles(as.matrix(VC), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Comunidad Valenciana"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Comunidad Valenciana"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_VC <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Comunidad Valenciana") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

EX <- read.table("Results/Extremadura_incidenceDEF.csv", header=T, sep=",")/100000*1067710
q2.5 <- colQuantiles(as.matrix(EX), probs=c(0.025))
med <- colMedians(as.matrix(EX))
q97.5 <- colQuantiles(as.matrix(EX), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Extremadura"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Extremadura"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_EX <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Extremadura") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

GA <- read.table("Results/Galicia_incidenceDEF.csv", header=T, sep=",")/100000*2699499
q2.5 <- colQuantiles(as.matrix(GA), probs=c(0.025))
med <- colMedians(as.matrix(GA))
q97.5 <- colQuantiles(as.matrix(GA), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Galicia"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Galicia"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_GA <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Galicia") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

IB <- read.table("Results/Islas Baleares_incidenceDEF.csv", header=T, sep=",")/100000*1149460
q2.5 <- colQuantiles(as.matrix(IB), probs=c(0.025))
med <- colMedians(as.matrix(IB))
q97.5 <- colQuantiles(as.matrix(IB), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Islas Baleares"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Islas Baleares"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_IB <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Islas Baleares") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

RI <- read.table("Results/La Rioja_incidenceDEF.csv", header=T, sep=",")/100000*316798
q2.5 <- colQuantiles(as.matrix(RI), probs=c(0.025))
med <- colMedians(as.matrix(RI))
q97.5 <- colQuantiles(as.matrix(RI), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="La Rioja"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="La Rioja"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_RI <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("La Rioja") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

MD <- read.table("Results/Madrid_incidenceDEF.csv", header=T, sep=",")/100000*6663394
q2.5 <- colQuantiles(as.matrix(MD), probs=c(0.025))
med <- colMedians(as.matrix(MD))
q97.5 <- colQuantiles(as.matrix(MD), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Madrid"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Madrid"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_MD <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Madrid") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

PV <- read.table("Results/País Vasco_incidenceDEF.csv", header=T, sep=",")/100000*2207776
q2.5 <- colQuantiles(as.matrix(PV), probs=c(0.025))
med <- colMedians(as.matrix(PV))
q97.5 <- colQuantiles(as.matrix(PV), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="País Vasco"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="País Vasco"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_PV <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("País Vasco") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

AS <- read.table("Results/Principado de Asturias_incidenceDEF.csv", header=T, sep=",")/100000*1022800
q2.5 <- colQuantiles(as.matrix(AS), probs=c(0.025))
med <- colMedians(as.matrix(AS))
q97.5 <- colQuantiles(as.matrix(AS), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Principado de Asturias"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Principado de Asturias"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_AS <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Principado de Asturias") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

MC <- read.table("Results/Región de Murcia_incidenceDEF.csv", header=T, sep=",")/100000*1493898
q2.5 <- colQuantiles(as.matrix(MC), probs=c(0.025))
med <- colMedians(as.matrix(MC))
q97.5 <- colQuantiles(as.matrix(MC), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Región de Murcia"]
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med[cases$CCAA=="Región de Murcia"]
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))
graph_MC <- ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Región de Murcia") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")

### Figure 1
ggarrange(graph_AN, graph_AR, graph_CN, graph_CB,
            graph_CM, graph_CL, graph_CT, graph_NC,
            graph_VC, graph_EX, graph_GA, graph_IB,
            graph_RI, graph_MD, graph_PV, graph_AS, graph_MC,
            ncol = 4, nrow = 5)

### Global graph (Figure 2)
cases <- cases %>% group_by(Week) %>% summarise(med=sum(med))
ESP <- AN+AR+CN+CB+CM+CL+CT+NC+VC+EX+GA+IB+RI+MD+PV+AS+MC

q2.5 <- colQuantiles(as.matrix(ESP), probs=c(0.025))
med <- colMedians(as.matrix(ESP))
q97.5 <- colQuantiles(as.matrix(ESP), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week
resum$Value <- "Estimated"
resum$med[44:86] <- cases$med
resum$Value[44:86] <- "Registered"
resum$q2.5[44:86] <- resum$med[44:86]
resum$q97.5[44:86] <- resum$med[44:86]
resum$med2 <- c(resum$med[1:43], rep(NA, 43))

ggplot(data=resum, aes(x=Week, y=med, col=Value)) +
  geom_line()+xlab("Week")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Spain") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Week, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")+ggtitle("Covid-19 cases in Spain")

### Global values
est <- sum(resum$med[resum$Value=="Estimated"])
reg <- sum(resum$med[resum$Value=="Registered"])
reg/est*100
