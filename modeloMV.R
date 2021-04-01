

##############################################
## Programa optimizacion Media-Varianza
## Con cortos (permite pesos negativos): 1
## Sin cortos (no pesos negativos): 0
##############################################

modeloMV <- function(clasif.sharpe){
    ret <- clasif.sharpe
    rf = rf
    n = n.act
    mu <- colMeans(ret)
    cov <- cov(ret)
    var <- diag(cov)
    sigma <- sqrt(var)
    activos <- names(ret)
    
    # Sin restricciones en corto
    if(short == 1){
        ones <- rep(1,n)
        x <- t(mu)%*%solve(cov)%*%mu
        y <- t(mu)%*%solve(cov)%*%ones
        z <- t(ones)%*%solve(cov)%*%ones
        d <- x*z - y*y
        g <- (solve(cov,ones)%*%x-solve(cov,mu)%*%y)%*%solve(d)
        h <- (solve(cov,mu)%*%z-solve(cov,ones)%*%y)%*%solve(d)
        rpmin <- min(mu)
        rpmax <- max(mu)
        nport <- 100
        j <- seq(rpmin,rpmax, length=nport) 
        wpo <- matrix(c(0), ncol=n, nrow=nport) 
        rpo <- matrix(c(0), nrow=nport)
        sigmapo <- matrix(c(0), nrow=nport)
        wj <- 0
        cont <- 1
        
        for(i in 1:nport){
            wj <- g + h*j[i] 
            wpo[cont,] <- t(wj)
            rpo[cont,] <- t(wj)%*%mu
            sigmapo[cont,] <- sqrt(t(wj)%*%cov%*%wj)
            cont <- cont+1
        }
        
        # PMVG
        cov_inv_1 <- solve(cov, ones) 
        wpmvg <- (1/as.numeric(ones %*% cov_inv_1)) * cov_inv_1
        rpmvg <-mu%*%wpmvg
        sigmapmvg <- sqrt(t(wpmvg)%*%cov%*%wpmvg)
        
        # Sharpe
        Er <- mu-rf 
        Z <- solve(cov,Er)  
        sumZ <- sum(Z) 
        wpt <- Z/sumZ 
        rpt <- t(wpt)%*%mu
        sigmapt <- sqrt(t(wpt)%*%cov%*%wpt)
        
        MV <- list()
        MV[[1]] <- wpo
        MV[[2]] <- rpo
        MV[[3]] <- sigmapo
        MV[[4]] <- wpmvg
        MV[[5]] <- rpmvg
        MV[[6]] <- sigmapmvg
        MV[[7]] <- wpt
        MV[[8]] <- rpt 
        MV[[9]] <- sigmapt
        return(MV)
    }
    # Con restricciones en corto
    else {
        pspec <- portfolio.spec(assets = activos)
        pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1)
        pspec <- add.objective(portfolio=pspec,
                               type='risk',
                               name='var')
        ef <- create.EfficientFrontier(R = ret, portfolio = pspec,
            type = "mean-sd", n.portfolios = 100)
        # Extraccion de portafolios
        port_optim <- cbind(ef[["frontier"]])
        rpo <- c(port_optim[,"mean"])
        sigmapo <- c(port_optim[,"StdDev"])
        wpo <- port_optim[,4:ncol(port_optim)]
        pminvar <- cbind(port_optim[1,])
        wpmvg <- cbind(round(pminvar[4:length(pminvar)],4))
        rownames(wpmvg) <- c(activos)
        rpmvg <-mu%*%wpmvg
        sigmapmvg <- sqrt(t(wpmvg)%*%cov%*%wpmvg)
        sharpe_port <- (rpo-rf)/sigmapo
        sharpe <- cbind(sharpe_port,wpo)
        sharpe.sort <- sharpe[order(-sharpe[,1]),]
        sharpe.sel <- cbind(sharpe.sort[1,])
        wpt <- round(cbind(sharpe.sel[2:length(sharpe.sel)]),4)
        rownames(wpt) <- c(activos)
        rpt <- mu%*%wpt
        sigmapt <- sqrt(t(wpt)%*%cov%*%wpt)
        
        MV <- list()
        MV[[1]] <- wpo
        MV[[2]] <- rpo
        MV[[3]] <- sigmapo
        MV[[4]] <- wpmvg
        MV[[5]] <- rpmvg
        MV[[6]] <- sigmapmvg
        MV[[7]] <- wpt
        MV[[8]] <- rpt 
        MV[[9]] <- sigmapt
        return(MV)
    }
}




