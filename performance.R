
###############################################
## Evaluación de desempeño

performance <- function(ret.sharpe,ret.treynor,r.indice){
  ret <-  ret.sharpe
  ret.treynor <- ret.treynor
  r.indice <- r.indice
  precios.hist <- precios.hist
  t <- nrow(ret)
  
  wpmvg <- wpmvg
  wpt <-  wpt
  wpot <- wpot
  
  names <- rownames(wpot)
  clasif.treynor <- cbind(ret.treynor[,names[1:length(wpot)]])
  
  rport <- matrix(0,nrow=t,ncol=4)
  colnames(rport) <- c("PMVG","Sharpe","Treynor","Benchmark")
  
  vport <- matrix(0,nrow=t,ncol=4)
  colnames(vport) <- c("PMVG","Sharpe","Treynor","Benchmark")
  
  # Retornos
  # PMVG
  rpmvg <- ret%*%wpmvg
  rport[,1] <- rpmvg 
  #Sharpe
  rpsharpe <- ret%*%wpt
  rport[,2] <- rpsharpe 
  # Treynor
  rptreynor <- clasif.treynor%*%wpot
  rport[,3] <- rptreynor
  # Benchmark
  rport[,4] <- r.indice
  
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
  # Benchmark
  v.benchmark <- matrix(0, nrow=t)
  v.benchmark[1] <- valor
  for(i in 2:t){
    v.benchmark[i] <- v.benchmark[i-1]*exp(r.indice[i-1])
  }
  vport[,4] <- v.benchmark
  #vport <- cbind(vport,retornos[,1]) 
  #vport <- vport[,1:4]
  
  DH <- list()
  DH[[1]] <- vport
  DH[[2]] <- rport
  
  return(DH)
}
