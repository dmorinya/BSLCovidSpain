library(BSL)
library(ggplot2)
library(doParallel)
library(gdata)
library(ggpubr)
library(dplyr)
library(lubridate)
source("Estep.R")
source("Mstep.R")
source("EM.R")

ncores <- detectCores()
cl <- makeCluster(ncores)
registerDoParallel(cl)

logPrior <- function(theta)
{
  log(theta[2] > 0 & theta[2] < 1 & theta[3] > 0 & 
      theta[3] < 1 & theta[4] > 0 & theta[4] < 1 &
      theta[5] > 0)
}

sim <- function(theta, T)
{
  mu <- (exp(log(exp(theta[6])*exp(theta[7]))-log(exp(theta[6])+(exp(theta[7])-1)))-1)
  x  <- theta[1]+arima.sim(model=list(order=c(1,0,0), ar=theta[2]), rand.gen=function(n, ...) rnorm(n, mean=mu, sd=theta[5]), n=T)
  z  <- rbinom(T, 1, theta[3])
  y  <- x[1:T]*(1-z[1:T])+theta[4]*z[1:T]*x[1:T]
  return(y)
}

st <- function(z){ 
  s1=mean(z); s2=sd(z); s3=acf(z,plot=F)$acf[2] 
  s4=acf(z,plot=F)$acf[3]; s5=acf(z,plot=F)$acf[4]
  c(s1,s2,s3,s4,s5)}

cases <- read.xls("../Data/cases.xls", encoding="latin1")
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
pob <- read.xls("../Data/poblacio.xls", encoding="latin1")
cases <- merge(cases, pob, by=c("CCAA"))
cases$incid <- cases$cases2/cases$Pob*100000
cases <- cases[cases$Week>7, ]
cases <- cases[cases$CCAA!="Ceuta" & cases$CCAA!="Melilla", ]

#### The model has to be defined and ran for every CCAA (with the same initial values as defined below)
for (i in 1:length(table(cases$CCAA)))
{
  CA <- names(table(cases$CCAA))[i]
  print(CA)
  addTaskCallback(function(...) {set.seed(123);TRUE})
  model <- BSLModel(fnSim = sim, fnSum = st,
                    simArgs = list(T = 50), 
                    theta0 = c(mean(cases$incid[cases$CCAA==CA])/0.9, 0.6, 0.5, 0.9, sd(cases$incid[cases$CCAA==CA])/0.9, 5.2, 0.3),
                    fnLogPrior = logPrior, thetaNames=c(expression(phi[0]), expression(alpha[1]), 
                    expression(omega), "q", expression(sigma), "m", expression(beta)))
  resultCovid <- bsl(y = cases$incid[cases$CCAA==CA], n = 500, M = 50000, model = model,
                     diag(c(.01^2,.005^2,.005^2,.005^2,.01^2,.01^2,.01^2)),
                     method = 'BSL', parallel=FALSE, verbose = FALSE)

  ### Parameter estimates and 95% credible intervals
  print(show(resultCovid))
  
  ### Parameter estimates distribution
  est_distr <- paste0("../Results/est_distr_", CA, ".pdf")
  pdf(est_distr, width=8.5, height=6.5)
    plot(resultCovid, which = 2, thin = 30,
		   options.density = list(color = 'blue4', fill = 'blue', alpha = 0.5),
		   options.theme = list(panel.background = element_rect(fill = 'lightgrey'),
		   plot.margin = grid::unit(rep(0.05, 4), "npc")))
  dev.off()

  #### Hidden process reconstruction
  posterior_probs <- lapply(resultCovid@theta[, 4], FUN=function(x){EM(cases$incid[cases$CCAA==CA],2,1e-16,x)})
  ur_indicator    <- lapply(posterior_probs, FUN=function(x){ifelse(x$gamma[, 1] > x$gamma[, 2], 1, 0)})
  q <- resultCovid@theta[, 4]
  rec_values <- list()
  for (j in 1:length(ur_indicator))
  {
    den <- q[j]
    rec_values[[j]] <- ifelse(ur_indicator[[j]]==0, cases$incid[cases$CCAA==CA], 
                                                    cases$incid[cases$CCAA==CA]/den)
  }

  pr <- as.data.frame(do.call(rbind, rec_values))
  
  ### Save the data
  name_data <- paste0("../Results/", CA, "_incidence2.csv")
  write.csv(pr, name_data, row.names=FALSE)

  #nam  <- paste("graph", i, sep = "")
  #nam2 <- paste("graph", i, "_def", sep = "")
  #assign(nam, ggplot(data=rec_data,
  #             aes(x=Week, y=incid, col=Value)) +
  #             geom_line()+xlab("Date")+ylab("Incidence x 100,000"))#+geom_ribbon(aes(ymin = low95, ymax = upp95), fill = "lightcoral", show.legend=F)
  #assign(nam2, eval(parse(text=nam))+theme(axis.text.x = element_text(hjust = 1))+
  #          labs(col = "Value")+ggtitle(CA) + theme(plot.title = element_text(hjust = 0.5)))
}

#jpeg("../Results/Covid19_Spain.jpeg", width=2000, height=980)
#  ggarrange(graph1_def, graph2_def, graph3_def, graph4_def,
#            graph5_def, graph6_def, graph7_def, graph8_def,
#            graph9_def, graph10_def, graph11_def, graph12_def,
#            graph13_def, graph14_def, graph15_def, graph16_def, graph17_def,
#            ncol = 4, nrow = 5)
#dev.off()