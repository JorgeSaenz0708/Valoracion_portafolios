
#######################################
## Programa para importar precios: YF
#######################################

precios <- function(activos){ 
  fechai <- fechai
  fechaf <- fechaf
  precios <- xts()
  for(i in 1:length(activos)){
    tmp <- Ad(getSymbols(activos[i], from=fechai, to=fechaf, 
                         periodicity=periodicidad, auto.assign = FALSE))
    tmp <- na.approx(tmp, na.rm=FALSE)
    precios <- cbind(precios, tmp)
  }
  colnames(precios) <- activos 
  tclass(precios) <- "Date"
  print("Los precios han sido importados correctamente")
  return (precios)
}
