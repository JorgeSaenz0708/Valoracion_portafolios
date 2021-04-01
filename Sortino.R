
# Modelo de Sortino

h <- 0 
n.act <- 15 # No. activos
semiretornos <- pmin(retornos,h) #escoge el min
semicov <- cov(semiretornos)
semivar <- diag(semicov)
semisigma <- sqrt(semivar)

windows()
plot(sigma,mu, ylim=c(0,max(mu)*1.2), xlim=c(0,max(sigma)*1.2),
     main="Plano riesgo-retorno")
points(semisigma, mu,  col="blue") 


sortino.act <- (mu-rf)/semisigma
sort.act <- sortino.act[order(-sortino.act)]
names <- names(sort.act)
clasif.sortino <- cbind(retornos[,names[1:n.act]])


# Sortino <- function(){}
ret <- clasif.sortino
rf = rf
n = n.act
semiretornos <- pmin(ret,h) #escoge el min


s.mu <- colMeans(ret)
s.cov <- cov(semiretornos)
s.var <- diag(s.cov)
s.sigma <- sqrt(s.var)
activos <- names(ret)


# Sin restricciones en corto
#if(short == 1){
ones <- rep(1,n)
x <- t(s.mu)%*%solve(s.cov)%*%s.mu
y <- t(s.mu)%*%solve(s.cov)%*%ones
z <- t(ones)%*%solve(s.cov)%*%ones
d <- x*z - y*y
g <- (solve(s.cov,ones)%*%x-solve(s.cov,s.mu)%*%y)%*%solve(d)
h <- (solve(s.cov,s.mu)%*%z-solve(s.cov,ones)%*%y)%*%solve(d)
rpmin <- min(s.mu)
rpmax <- max(s.mu)
nport <- 100
j <- seq(rpmin,rpmax, length=nport) 
wpo <- matrix(c(0), ncol=n, nrow=nport) 
rpo <- matrix(c(0), nrow=nport)
s.sigmapo <- matrix(c(0), nrow=nport)
wj <- 0
cont <- 1

for(i in 1:nport){
  wj <- g + h*j[i] 
  wpo[cont,] <- t(wj)
  rpo[cont,] <- t(wj)%*%s.mu
  s.sigmapo[cont,] <- sqrt(t(wj)%*%s.cov%*%wj)
  cont <- cont+1
}

# Sortino
Er <- s.mu-rf 
Z <- solve(s.cov,Er)  
sumZ <- sum(Z) 
wps <- Z/sumZ #pesos portafolio sortino
rps <- t(wps)%*%s.mu
s.sigmaps <- sqrt(t(wps)%*%s.cov%*%wps)

# PS <- list()
# MV[[1]] <- wpo
# MV[[2]] <- rpo
# MV[[3]] <- s.sigmapo
# MV[[4]] <- wps
# MV[[5]] <- rps 
# MV[[6]] <- s.sigmaps
# return(PS)
#}

# Con restricciones en corto
#else {
# FE    
library(quadprog)
nport <- 100
rpmin = min(s.mu)*1.1
rpmax <- max(s.mu)*0.9
j <- round(seq(rpmin,rpmax,length=nport),4)
s.sigmapo <- matrix(0,nrow=nport)
wpo <- matrix(0,nrow=nport, ncol=n)
Amat <- t(rbind(s.mu,rep(1,n),diag(1,nrow=n)))
dvec <- rep(0,n) 
Dmat <- 2*s.cov
for(i in 1:nport){
  bvec <- c(j[i],1,rep(0,n))
  result <- solve.QP(Dmat,dvec,Amat,bvec,meq=2)
  wpo[i,] <- result$solution
  s.sigmapo[i,] <- sqrt(result$value)
}
rpo <- j
colnames(wpo) <- c(activos)
sum(wpo[5,])


# Sortino
sortino_port <- (rpo-rf)/s.sigmapo
sortino <- cbind(sortino_port,wpo)
sortino.sort <- sortino[order(-sortino[,1]),]
sortino.sel <- cbind(sortino.sort[1,])
wps <- t(round(cbind(sortino.sel[2:length(sortino.sel)]),4))
colnames(wps) <- c(activos)
rps <- s.mu%*%t(wps)
s.sigmaps <- sqrt(wps%*%s.cov%*%t(wps))

PS <- list()
MV[[1]] <- wpo
MV[[2]] <- rpo
MV[[3]] <- sigmapo
MV[[4]] <- t(wpt)
MV[[5]] <- rpt 
MV[[6]] <- sigmapt
return(PS)

windows()
plot(sigma,mu, ylim=c(0,max(mu)*1.2), xlim=c(0,max(sigma)*1.2),
     main="Plano riesgo-retorno")
points(semisigma, s.mu, col="blue") 
points(sigmapt,rpt, col="red")
points(s.sigmaps,rps, col="purple")
points(s.sigmapo,rpo, type="l", col="purple")


windows()
par(mfrow = c(2, 1))
barplot(t(wpt), main="Sharpe", axisnames = TRUE, beside = TRUE)
barplot(t(wps), main="Sortino",axisnames = TRUE, beside = TRUE)
