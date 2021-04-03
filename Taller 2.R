#Taller 2 

library(zoo)
library(xts)
library(quantmod)
library(quadprog)
library(PortfolioAnalytics)
library(ROI)
#require('ROI.plugin.glpk')
#require('ROI.plugin.quadprog')
#install.packages("devtools")
library(devtools)
#install.packages("remotes")
#library(remotes)

rm(list=ls())
#setwd("C:/Users/Samuel/OneDrive/Documents/Semestre 2021(1)/Valoración de portafolios")
#source("codes/precios.R")
#source("codes/modeloMV.R")
#source("codes/treynor.R")
#source("codes/sortino.R")
#source("codes/omega.R")
#source("codes/performance.R")
#source("codes/performancefm.R")

devtools::source_url("https://github.com/JorgeSaenz0708/Valoracion_portafolios/blob/main/precios.R?raw=TRUE")
devtools::source_url("https://github.com/JorgeSaenz0708/Valoracion_portafolios/blob/main/modeloMV.R?raw=TRUE")
devtools::source_url("https://github.com/JorgeSaenz0708/Valoracion_portafolios/blob/main/treynor.R?raw=TRUE")
devtools::source_url("https://github.com/JorgeSaenz0708/Valoracion_portafolios/blob/main/Sortino.R?raw=TRUE")
devtools::source_url("https://github.com/JorgeSaenz0708/Valoracion_portafolios/blob/main/omega_.R?raw=TRUE")
devtools::source_url("https://github.com/JorgeSaenz0708/Valoracion_portafolios/blob/main/performance_.R?raw=TRUE")
devtools::source_url("https://github.com/JorgeSaenz0708/Valoracion_portafolios/blob/main/performancefm_.R?raw=TRUE")

# Inputs
fechai <- '2010-01-01'
fechaf <- '2020-12-31'
periodicidad <- "monthly"

# Importar datos
activos <- c("NVDA", "INTC", "AVGO",  "QCOM", "TXN", "AMD",  "MU", "ADI","MSFT", 
             "ADBE", "ORCL", "SNPS", "VRSN", "FTNT", "AKAM", "FFIV", "GOOGL", "F", "MCD", "SBUX", "CMG", "YUM", "DRI", "DPZ", "XOM",                     
             "CVX","COP","EOG","PXD","OXY","HES","ACN","IBM","FIS","FISV","CTSH",
             "BR","V","MA","AXP","COF","DFS","JPM","BAC","C","WFC","MS",
             "SCHW","GS","MKTX","RJF","JNJ","PFE","LLY","MRK","BMY","GILD","KO","PEP","MNST")


precios.hist <- precios(activos)
retornos <- diff(log(precios.hist))[-1,]

mu <- (colMeans(retornos))
cov <- cov(retornos)
var <- diag(cov)
sigma <- sqrt(var)

# Indice
indice <- c("^GSPC")
indice.hist <- precios(indice)
r.indice <- diff(log(indice.hist))[-1,]

## Clasificacion de activos
rf <- 0 # rf anual
n.act <- 15 # No. activos
short <- 0    #(1: con cortos; 0: sin cortos)

sharpe.act <- (mu-rf)/sigma
sort.act <- sharpe.act[order(-sharpe.act)]
names <- names(sort.act)
clasif.sharpe <- cbind(retornos[,names[1:n.act]])

mu_ret_p <- colMeans(retornos[,names[1:n.act]])
cov_p <- cov(retornos[,names[1:n.act]])                   
var_p <- diag(cov_p)
sigma_p <- sqrt(var_p)

##### Modelo MV
MV <- modeloMV(clasif.sharpe)

## Markowitz
wpo <- MV[[1]]
rpo <- MV[[2]]
sigmapo <- MV[[3]]

#PMVG
wpmvg <- MV[[4]]
rpmvg <- MV[[5]]
sigmapmvg <- MV[[6]]

## Sharpe
wpt <- MV[[7]]
rpt <- MV[[8]] 
sigmapt <- MV[[9]]

## Treynor
POT <- treynor(retornos,r.indice)
wpot <- POT[[1]]
rpot <- POT[[2]]
sigmapot <- POT[[3]]
n.optimo <- POT[[4]]

# Sortino
h <- 0
semiretornos <- pmin(retornos,h)
semicov <- cov(semiretornos)
semivar <- diag(semicov)
semisigma <- sqrt(semivar)

sortino.act <- (mu-rf)/semisigma
sort.act <- sortino.act[order(-sortino.act)]
names <- names(sort.act)
clasif.sortino <- cbind(retornos[,names[1:n.act]])

PS <- sortino(clasif.sortino,h)
wps <- t(PS[[4]])
rps <- PS[[5]]
sigmaps <- PS[[6]]

# Omega

h <- 0
OM <- omega(retornos,h)
wpomega <- OM[[1]]
rpomega <- OM[[2]]
sigmapomega <- OM[[3]]
retpomega <- OM[[4]]

l <- retpomega %*% wpomega
#---------------------------------------------------------------
# Plano Riesgo-retorno
windows()
plot(sigma_p,mu_ret_p, ylim=c(0,max(mu_ret_p)*1.2), xlim=c(0,max(sigma_p)*1.2),
     main="Plano riesgo-retorno")
points(sigmapo,rpo, type="l", col="blue") 
points(sigmapt,rpt, col="red")
points(sigmapot,rpot, col="red")
points(sigmaps,rps, col="darkgreen")
points(sigmapmvg,rpmvg, col="purple")
points(sigmapomega,rpomega, col="darkgray")
text(sigmapt,rpt,labels="PT",pos = 2, cex=1)
text(sigmapmvg,rpmvg,labels="PMVG",pos = 2, cex=0.7)
text(sigmapot,rpot,labels="POT",pos = 2, cex=1)
text(sigmaps,rps,labels="PS",pos = 2, cex=1)
text(sigmapomega,rpomega,labels="OM",pos = 2, cex=1)

#-----------------------------------------------------------------
#-----------------------------------------------------------------
# Pesos óptimos

windows()
par(mfrow = c(2, 3))
barplot(t(wpt), main="Sharpe", axisnames = TRUE, beside = TRUE)
barplot(t(wpmvg), main="PMVG",axisnames = TRUE, beside = TRUE)
barplot(t(wpot[1:n.optimo,]), main="Treynor", axisnames = TRUE, beside = TRUE)
barplot(t(wps), main="Sortino", axisnames = TRUE, beside = TRUE)
barplot(t(wpomega), main="Omega", axisnames = TRUE, beside = TRUE)

#----------------------------------------------------------------
#-----------------------------------------------------------------

# Desempeño In-sample
valor <- 100 # Valor inicial del portafolio 
DH <- performance(clasif.sharpe,retornos,clasif.sortino,retpomega,r.indice)
Performance <-ts(DH[[1]],start=2010, frequency=12)

windows()
plot(Performance[,"PMVG"], col='blue', type='l', ylim=c(0,max(Performance)), 
     main="Evaluación de desempeño", ylab = "Valor")
lines(Performance[,"Sharpe"], col='black')
lines(Performance[,"Treynor"], col='purple')
lines(Performance[,"Sortino"], col='darkgreen')
lines(Performance[,"Omega"], col='orange')
lines(Performance[,"Benchmark"], col='darkgray')
legend("topleft",c("PMVG","Sharpe","Treynor","Sortino","Omega","Benchmark"),
       fill=c("blue","black","purple","darkgreen","orange","darkgray"))

# --------------------------------------------------------------
# Tabla resumen:

Resumen <- matrix(0,4,6)
rp.hist <- DH[[2]]
rp <- round(rbind(rpmvg,rpomega,rpt,rpot,rps,mean(r.indice)),4)
riesgop <- round(rbind(sigmapmvg,sigmapt,sd(rp.hist[,3]),
                       sd(rp.hist[,4]),sigmapomega,sd(r.indice)),4)
sharpep <- round(rbind((rpmvg-rf)/sigmapmvg,
                       (rpt-rf)/sigmapt,
                       (rpot-rf)/sd(rp.hist[,3]),
                       (rps-rf)/sd(rp.hist[,4]),
                       (rpomega-rf)/sigmapomega,
                       (mean(r.indice)-rf)/sd(r.indice)),4)

drawdown <- round(rbind(min(rp.hist[,1]),min(rp.hist[,2]),
                        min(rp.hist[,3]),min(rp.hist[,4]),
                        min(rp.hist[,5]),min(rp.hist[,6])),4)

Resumen <- cbind(sharpep,rp,riesgop,drawdown)
colnames(Resumen) <- c("Sharpe","Retorno","Riesgo","Drawdown")
rownames(Resumen) <- c("PMVG","PT","POT","PS","POM","Indice")

Resumen

#-----------------------------------------------------------------
#-----------------------------------------------------------------

# Desempeño Out-sample

fechai <- '2019-12-01'
fechaf <- '2020-12-31'

preciosfm <- precios(activos)
retornosfm <- diff(log(preciosfm))[-1,]
indicefm <- precios(indice)
r.indice <- diff(log(indicefm))[-1,]

valor <- 100 # Valor inicial del portafolio 
DFM <- performancefm(wpmvg,wpt,wpot,wps,wpomega,retornosfm,indice)

Performance <-ts(DFM[[1]],start=2020, frequency=12)

windows()
plot(Performance[,"PMVG"], col='blue', type='l', 
     ylim=c(min(Performance)*0.9,max(Performance)), 
     main="Evaluación de desempeño", ylab = "Valor")
lines(Performance[,"Sharpe"], col='black')
lines(Performance[,"Treynor"], col='purple')
lines(Performance[,"Sortino"], col='darkgreen')
lines(Performance[,"Omega"], col='orange')
lines(Performance[,"Benchmark"], col='darkgray')
legend("topleft",c("PMVG","Sharpe","Treynor","Sortino","Omega","Benchmark"),
       fill=c("blue","black","purple","darkgreen","orange","darkgray"))
