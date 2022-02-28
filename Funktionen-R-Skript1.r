## Deskriptive statistiken fuer metrische merkmale
functionA <- function(x){
  medianx = median(x)
  meanx = mean(x)
  varx = var(x)
  maxx = max(x)
  minx = min(x)
  
  cat(paste("Median: ",toString(medianx),"\n"))
  cat(paste("Mean: ",toString(meanx),"\n"))
  cat(paste("Varianz: ",toString(varx),"\n"))
  cat(paste("Minimum: ",toString(minx),"\n"))
  cat(paste("Maximum: ",toString(maxx),"\n"))
  cat("10-, 25-, 75- und 90% Quantile: \n")
  cat(paste(toString(quantile(x,0.1)),"\n"))
  cat(paste(toString(quantile(x,0.25)),"\n"))
  cat(paste(toString(quantile(x,0.75)),"\n"))
  cat(paste(toString(quantile(x,0.90)),"\n"))
}

## Deskriptive statistiken fuer kategorielle merkmale
functionB <- function(x){
  modex = getmode(x) ## aus Funktionen-R-Skript2
  cat(paste("Modus: ",toString(modex),"\n"))
  cat("Haeufigkeiten: \n")
  table(x)
}

