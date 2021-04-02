
# --------------------------------------------
## Programa de optimización Omega
# --------------------------------------------

omega <- function(retornos,h){
# Cargar paquetes
    library(ROI)
    library(ROML)
    library(ROML.portfolio)
    # Inputs
    retornos <- retornos
    n <- ncol(retornos)
    h <- h
    n.act <- n.act
    omegai <- rep(0,n)
    for(j in 1:n){
        omegai[j]<- sum(pmax(retornos[,j]-h,0))/sum(pmax(h-retornos[,j],0))
    }
    names(omegai) <- colnames(retornos)
    omegasort <- omegai[order(-omegai)]
    names <- names(omegasort)
    clasif.omega <- cbind(retornos[,names[1:n.act]])
##---------------------------------------------------------------------
    # Optimización
if(short == 1){
    ret.omega <- coredata(clasif.omega)
    m <- model()
    m$variable(portfolio, lb = -1) 
    m$maximize( omega(portfolio) )
    opt <- optimize(m, solver="glpk",data=list(returns = ret.omega)) 
    wpomega <- round(opt$solution[grep("portfolio", names(opt$solution))]/
              opt$solution[grep("z", names(opt$solution))], 4)
    names(wpomega) <- names(clasif.omega)
    wpomega <- t(wpomega)
    muo <- colMeans(clasif.omega)
    covo <- cov(clasif.omega)    
    rpomega <- muo%*%t(wpomega)
    sigmapomega <- sqrt(wpomega%*%covo%*%t(wpomega))
    ret.po <- clasif.omega%*%t(wpomega)
    omegap <- sum(pmax(ret.po-h,0))/sum(pmax(h-ret.po,0))
    
    OM <- list()
    OM[[1]] <- t(wpomega)
    OM[[2]] <- rpomega
    OM[[3]] <- sigmapomega
    OM[[4]] <- ret.po
    OM[[5]] <- omegap
    return(OM)
}
    else {
        ret.omega <- coredata(clasif.omega)
        m <- model()
        m$variable(portfolio, lb = 0) 
        m$maximize( omega(portfolio) )
        opt <- optimize(m, solver="glpk",data=list(returns = ret.omega)) 
        wpomega <- round(opt$solution[grep("portfolio", names(opt$solution))]/
                             opt$solution[grep("z", names(opt$solution))], 4)
        names(wpomega) <- names(clasif.omega)
        wpomega <- t(wpomega)
        muo <- colMeans(clasif.omega)
        covo <- cov(clasif.omega)    
        rpomega <- muo%*%t(wpomega)
        sigmapomega <- sqrt(wpomega%*%covo%*%t(wpomega))
        ret.po <- clasif.omega%*%t(wpomega)
        omegap <- sum(pmax(ret.po-h,0))/sum(pmax(h-ret.po,0))
        
        OM <- list()
        OM[[1]] <- t(wpomega)
        OM[[2]] <- rpomega
        OM[[3]] <- sigmapomega
        OM[[4]] <- ret.po
        OM[[5]] <- omegap
        return(OM)
    }
}
