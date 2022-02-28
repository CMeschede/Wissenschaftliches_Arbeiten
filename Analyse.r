## skript zur analyse des Datensatzes, benoetigt Funktionen-R-Skript1 und 2 und den Datensatz "daten.csv", benannt "daten" im code


functionA(daten$alter)

## Deskriptive Statistiken fuer "Alter":
##
##  Median: 24
##  Arr.Mittel: 24,71
##  Varianz: ~4 => SD: ~2
##  Minimum: 20
##  Maximum: 30
##  Quantile:
##  10%: 23
##  25%: 23
##  75%: 26
##  90%: 28
##

## Merkmal ist von 1-7 mit 1 = geringes interesse,7 = sehr hohes Interesse
functionA(daten$inter_math)

## Deskriptive Statistiken fuer "Interesse an Mathe":
##
##  Median: 3
##  Arr.Mittel: 3,21
##  Varianz: ~2,7 => SD: ~1,64
##  Minimum: 1
##  Maximum: 7
##  Quantile: 
##  10%: 1
##  25%: 2
##  75%: 5
##  90%: 5
##


##vergleich von Mathe interesse bei personen die MatheLK haben
functionA(daten$inter_math[which(daten$math_lk == "Ja")])

##vergleich von Mathe interesse bei personen die keinen MatheLK haben
functionA(daten$inter_math[which(daten$math_lk == "Nein")])

##
##  Arr.Mittel:
##  LK: 3,51    kein LK: 2,51
##  
##  Median: 
##  LK: 4       Kein LK: 2
##
##
## => leute mit Mathe LK haben größeres interesse an Mathe als leute ohne mathe LK
##


##  analyse des interesses am programmieren bei leuten, die keines an Mathe haben
functionB(functionE(daten$inter_prog[
  which(functionE(daten$inter_math) == "niedrig")]))

##  analyse des interesses am programmieren bei leuten, die 
##  mittleres bis hohes an Mathe haben
functionB(functionE(daten$inter_prog[
  which(!(functionE(daten$inter_math) == "niedrig"))]))

## keine person hat ein hohes interesse am Programmieren, wenn sie ein niedriges 
## interesse an Mathe hat

## interesse an Mathe und Programmieren scheint warscheinlich, weitere Analyse 
## Notwendig


