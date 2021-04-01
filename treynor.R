
#######################################
## Programa Óptimización Treynor
## Permite solo Largos (pesos positivos)

treynor <- function(retornos,rindice){
    rf <- rf
    n.act <- n.act
    mu <- (colMeans(retornos))
    cov <- cov(retornos)
    var <- diag(cov)
    sigma <- sqrt(var)
    nret <- length(retornos[,1])
    n <- length(mu)
    betas <- matrix(0,ncol=n)
    
    f.betas <- function(retornos,r.indice){
        # Si datos > 60
        n.datos <- length(retornos[,1])
        corte <- n.datos - 59
        if(n.datos>60){
            retornos60 <- retornos[corte:n.datos,]
            r.indice60 <- r.indice[corte:n.datos,]
            for(i in 1:n){
                capm <- lm(retornos60[,i]~r.indice60)
                coef <- capm[["coefficients"]]
                betas[,i] <- coef[2]
            }
        }
        else{
            # Sin datos < 60
            for(i in 1:n){
                capm <- lm(retornos[,i]~r.indice)
                coef <- capm[["coefficients"]]
                betas[,i] <- coef[2]
            }
            print("Advertencia: estas trabajando con menos de 60 datos")
        }
        return(betas)
    }
    
    betas <- f.betas(retornos,r.indice)
    sigmaindice <- sd(r.indice)
    varerror <- var - betas^2*sigmaindice^2
    treynor <- (mu-rf)/betas
    matrix <- t(rbind(treynor,t(mu),t(sigma),betas,varerror))
    sort.matrix <- matrix[order(-matrix[,1]),]
    ratio1 <- ((sort.matrix[,2]-rf)*sort.matrix[,4])/sort.matrix[,5]
    ratio2 <- sort.matrix[,4]^2/sort.matrix[,5]
    sumacu1 <- cumsum(ratio1)
    sumacu2 <- cumsum(ratio2)
    coef.c <- (sigmaindice^2*sumacu1)/(1+sigmaindice^2*sumacu2)
    diff <- sort.matrix[,1]-coef.c
    cond.diff <- diff[!is.na(diff)&diff>0]
    n.optimo <- length(cond.diff)
    cuttoff <- coef.c[n.optimo]
    
    Zi <- (sort.matrix[,4]/sort.matrix[,5])*(sort.matrix[,1]-cuttoff)
    Zi <- pmax(Zi,0)
    wi <- Zi/sum(Zi)
    rpot <- t(wi)%*%mu
    sigmapot <- sqrt(t(wi)%*%cov%*%wi)
    wi <- cbind(wi)
    # if(n.optimo < n.act){
    #     wi <- wi[1:n.act]
    # }
    # else{
    #     wi <- wi[!is.na(wi)&wi>0]
    # }
    POT <- list()
    POT[[1]] <- wi
    POT[[2]] <- rpot
    POT[[3]] <- sigmapot
    return(POT)    
}
    

    