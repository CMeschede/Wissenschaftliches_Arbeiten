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

functionC <- function(daten){
  #deskriptive bivariate Statistik zwischen Kategorien
  #benötigt die ganze CSV Datei
  matrix_fach  <- model.matrix(~ fach -1 , daten)
  matrix_math_lk  <- model.matrix(~ math_lk -1 , daten)
  matrix_inter_prog <- model.matrix(~ inter_prog -1, daten)
  matrix_inter_math <-model.matrix(~ inter_math-1, daten)
  
  print("Korrelationsmatritzen: ")
  cat("\n")
  print(cor(matrix_fach, matrix_inter_math))
  cat("\n")
  print(cor(matrix_fach, matrix_inter_prog))
  cat("\n")
  print(cor(matrix_fach, matrix_math_lk))
}

#Funktion D, +Korrelation zwischen metrisch und Dichtonomen Variable
functionD <- function(metrisch, dichto){
  dichto_transform <- as.numeric(x)
  print(cor(metrisch, dichto_transform))
}

#Funktion E teilt ordinale daten in drei 0-33%, 34-66%, 67-100%
#Quantile auf und labelt sie "niedrig", "mittel" und "hoch"
functionE <- function(vector){

  result <- cut(vector,c(min(vector)-1,floor(quantile(vector, 0.33)),
                         floor(quantile(vector, 0.66)),max(vector)+1),
                         labels=c("niedrig","mittel","hoch"))
  return(result)
}

#Funktion F erstellt eine geeignete Visualisierung von drei oder vier kategorialen Variablen
library(ggplot2) =
#Histogramm erstellen
functionF <- function(daten) {
   ggplot(daten, aes(x=reorder(data, data, function(x)-length(x)))) + geom_bar(fill='blue') +  labs(x = 'Histogram of Data')
}                            

