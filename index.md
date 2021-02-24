---
title: "My Website"


output: 
  html_document:
    keep_md: true
    
---

# Notas del curso de Inferencia, Unalmed


## Usaré el dataset ´survey´ para dar un uso práctico a toda la teoria 

```r
library(MASS)
data(survey)
head(survey)
```

```
##      Sex Wr.Hnd NW.Hnd W.Hnd    Fold Pulse    Clap Exer Smoke Height      M.I
## 1 Female   18.5   18.0 Right  R on L    92    Left Some Never 173.00   Metric
## 2   Male   19.5   20.5  Left  R on L   104    Left None Regul 177.80 Imperial
## 3   Male   18.0   13.3 Right  L on R    87 Neither None Occas     NA     <NA>
## 4   Male   18.8   18.9 Right  R on L    NA Neither None Never 160.00   Metric
## 5   Male   20.0   20.0 Right Neither    35   Right Some Never 165.00   Metric
## 6 Female   18.0   17.7 Right  L on R    64   Right Some Never 172.72 Imperial
##      Age
## 1 18.250
## 2 17.583
## 3 16.917
## 4 20.333
## 5 23.667
## 6 21.000
```


# Estadisticos de orden


```r
#1. Ordenar los datos

pulsos <- sort(survey[!is.na(survey$Pulse),]$Pulse)

#2. Contar numero de datos

n <- length(pulsos)
sprintf("numero de datos: %s", n)
```

```
## [1] "numero de datos: 192"
```

```r
#3. Rango
# Xn - X1

sprintf("rango: %s", pulsos[n] - pulsos[1])
```

```
## [1] "rango: 69"
```

```r
#4. Mediana
#Como n es par:

mediana <- pulsos[(n+1)/2]

#Si fuera impar
#mediana <- pulsos[n/2]+pulsos[(n/2) + 1]

sprintf("mediana: %s", mediana)
```

```
## [1] "mediana: 72"
```

```r
#5. Percentil
p <- 0.25

#n*p
#Si n*p es entero

percentil <- (pulsos[n*p] + pulsos[n*p + 1])/2

#np siendo parte decimal de n*p
#percentil <- pulsos[np]
print(paste("percentil", p, "es:", percentil, sep=" "))
```

```
## [1] "percentil 0.25 es: 66"
```

