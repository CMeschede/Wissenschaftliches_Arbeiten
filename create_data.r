# Aufgabe 1

interess_math <- function(fach,len_data) {
  inter_math <- 1:len_data
  for (i in 1:len_data) {
    if(fach[i] == "Mathe") inter_math[i] = sample(6:7, 1)
    else if (fach[i] == "Statistik") inter_math[i] = sample(4:5, 1)
    else if (fach[i] == "Data Science") inter_math[i] = sample(2:3, 1)
    else if (fach[i] == "Informatik") inter_math[i] = 1
  }
  return(inter_math)
}

interess_prog <- function(fach,len_data) {
  inter_prog <- 1:len_data
  for (i in 1:len_data) {
    if(fach[i] == "Informatik") inter_prog[i] = sample(6:7, 1)
    else if (fach[i] == "Data Science") inter_prog[i] = sample(4:5, 1)
    else if (fach[i] == "Statistik") inter_prog[i] = sample(2:3, 1)
    else if (fach[i] == "Mathe") inter_prog[i] = 1
  }
  return(inter_prog)
}

mathe_lk <- function(fach,len_data) {
  math_lk <- 1:len_data
  for (i in 1:len_data) {
    if(fach[i] == "Mathe") math_lk[i] = sample(c("Ja", "Nein"), 1,
                                               prob = c(0.9, 0.1))
    else if (fach[i] == "Statistik") math_lk[i] = sample(c("Ja", "Nein"), 1,
                                                         prob = c(0.8, 0.2))
    else if (fach[i] == "Data Science") math_lk[i] = sample(c("Ja", "Nein"), 1, 
                                                            prob = c(0.7, 0.3))
    else if (fach[i] == "Informatik") math_lk[i] = sample(c("Ja", "Nein"), 1, 
                                                          prob = c(0.6, 0.4))
  }
  return(math_lk)
}


# Default länge für das DataFrame
len_data = 100

set.seed(1234)
alter <- round(rnorm(len_data, 25, 2))
fach <- sample(c("Statistik", "Data Science", "Mathe", "Informatik"),
               len_data, replace = TRUE, prob = c(0.35, 0.35, 0.1, 0.2))

inter_math <- interess_math(fach,len_data)
inter_prog <- interess_prog(fach,len_data)
math_lk <- mathe_lk(fach,len_data)

daten <- data.frame(alter, fach, inter_math, inter_prog, math_lk)
