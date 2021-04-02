
## -------------------------------------------------
## Evaluación de desempeño - Out sample
## -------------------------------------------------

performancefm <- function(wpmvg,wpt,wpot,wps,wpomega,retornosfm,indicefm){
    retornosfm <- retornosfm
    wpmvg <- wpmvg
    wpt <-  wpt
    wpot <- t(t(wpot[1:n.optimo,]))
    wps <- wps
    wpomega <- wpomega
    
    namesMV <- rownames(wpt)
    namesPOT <- rownames(wpot) 
    namesPS <- rownames(wps) 
    namesOM <- rownames(wpomega) 
    t <- nrow(retornosfm)
    
    ret.sharpe <- cbind(retornosfm[,namesMV[1:length(wpt)]])
    ret.treynor <- cbind(retornosfm[,namesPOT[1:length(wpot)]])
    ret.sortino <- cbind(retornosfm[,namesPS[1:length(wps)]])
    ret.omega <- cbind(retornosfm[,namesOM[1:length(wpomega)]])
    
    rport <- matrix(0,nrow=t,ncol=6)
    colnames(rport) <- c("PMVG","Sharpe","Treynor","Sortino","Omega","Benchmark")
    
    vport <- matrix(0,nrow=t,ncol=6)
    colnames(vport) <- c("PMVG","Sharpe","Treynor","Sortino","Omega","Benchmark")
    
    # Retornos
    # PMVG
    rpmvg <- ret.sharpe%*%wpmvg
    rport[,1] <- rpmvg 
    #Sharpe
    rpsharpe <- ret.sharpe%*%wpt
    rport[,2] <- rpsharpe 
    # Treynor
    rptreynor <- ret.treynor%*%wpot
    rport[,3] <- rptreynor
    #Sortino
    rpsortino <- ret.sortino%*%wps
    rport[,4] <- rpsortino
    #Omega
    rpomega <- ret.omega%*%wpomega
    rport[,5] <- rpomega
    # Benchmark
    rport[,6] <- r.indice
    
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
