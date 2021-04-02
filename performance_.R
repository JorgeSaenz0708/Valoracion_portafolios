
## -------------------------------------------------
## Evaluación de desempeño - In sample
## -------------------------------------------------

performance <- function(ret.sharpe,ret.treynor,r.sortino,retpomega,r.indice){
  ret <-  ret.sharpe
  ret.treynor <- ret.treynor
  r.indice <- r.indice
  r.sortino <- r.sortino
  precios.hist <- precios.hist
  retpomega <- retpomega
  t <- nrow(ret)
  
  wpmvg <- wpmvg
  wpt <-  wpt
  wpot <- wpot
  wps <- wps
  wpomega <- wpomega
  
  names <- rownames(wpot)
  clasif.treynor <- cbind(ret.treynor[,names[1:length(wpot)]])
  
  rport <- matrix(0,nrow=t,ncol=6)
  colnames(rport) <- c("PMVG","Sharpe","Treynor","Sortino","Omega","Benchmark")
  
  vport <- matrix(0,nrow=t,ncol=6)
  colnames(vport) <- c("PMVG","Sharpe","Treynor","Sortino","Omega","Benchmark")
  
  # Retornos
  # PMVG
  rpmvg <- ret%*%wpmvg
  rport[,1] <- rpmvg 
  #Sharpe
  rpsharpe <- ret%*%wpt
  rport[,2] <- rpsharpe 
  # Treynor
  rptreynor <- ret.treynor%*%wpot
  rport[,3] <- rptreynor
  #Sortino
  rpsortino <- r.sortino%*%wps
  rport[,4] <- rpsortino
  # Omega
  rpomega <- retpomega
  rport[,5] <- rpomega
  # Benchmark
  rport[,6] <- mean(r.indice)
  
  # Indexación
  #rport <- cbind(rport,retornos[,1]) 
  #rport <- rport[,1:4]
  
  # Valor del portafolio
  # PMVG
  port.mvg <- matrix(0, nrow=t)
  port.mvg[1] <- valor
  for(i in 2:t){
    port.mvg[i] <- port.mvg[i-1]*exp(rpmvg[i-1])
  }
  vport[,1] <- port.mvg
  # Sharpe
  port.sharpe <- matrix(0, nrow=t)
  port.sharpe[1] <- valor
  for(i in 2:t){
  port.sharpe[i] <- port.sharpe[i-1]*exp(rpsharpe[i-1])
  }
  vport[,2] <- port.sharpe
  # Treynor
  port.treynor <- matrix(0, nrow=t)
  port.treynor[1] <- valor
  for(i in 2:t){
    port.treynor[i] <- port.treynor[i-1]*exp(rptreynor[i-1])
  }
  vport[,3] <- port.treynor
  # Sortino
  port.sortino<- matrix(0, nrow=t)
  port.sortino[1] <- valor
  for(i in 2:t){
    port.sortino[i] <- port.sortino[i-1]*exp(rpsortino[i-1])
  }
  vport[,4] <- port.sortino
  # Omega
  port.omega<- matrix(0, nrow=t)
  port.omega[1] <- valor
  for(i in 2:t){
    port.omega[i] <- port.omega[i-1]*exp(rpomega[i-1])
  }
  vport[,5] <- port.omega
  # Benchmark
  v.benchmark <- matrix(0, nrow=t)
  v.benchmark[1] <- valor
  for(i in 2:t){
    v.benchmark[i] <- v.benchmark[i-1]*exp(r.indice[i-1])
  }
  vport[,6] <- v.benchmark
  #vport <- cbind(vport,retornos[,1]) 
  #vport <- vport[,1:4]
  
  DH <- list()
  DH[[1]] <- vport
  DH[[2]] <- rport
  
  return(DH)
}
