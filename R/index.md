---
title: "Notas del curso de Inferencia, Unalmed"


output: 
  html_document:
    keep_md: true
    
---

# Datos
Usaré el dataset ´survey´ para dar un uso práctico a toda la teoria 

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


#Si n*p es entero

percentil <- (pulsos[n*p] + pulsos[n*p + 1])/2

#Si n*p no es entero
#np siendo parte decimal de n*p
#percentil <- pulsos[np + 1]

print(paste("percentil", p, "es:", percentil, sep=" "))
```

```
## [1] "percentil 0.25 es: 66"
```

# TLC


```r
tlcbin <- function(p) 
{
    n <- c(10, 15, 20, 30, 60, 100, 200, 500, 1000)
    mat <- matrix(0, 1000, length(n))
    for (j in 1:length(n)) {
        for (i in 1:1000) {
            mat[i, j] <- (rbinom(1, n[j], p) - n[j] * p)/sqrt(n[j] * 
                p * (1 - p))
        }
    }
    par(mfrow = c(3, 3))
    hist(mat[, 1], xlab = "Medias", main = "Histograma para Medias, n=10", 
        freq = FALSE)
    hist(mat[, 2], xlab = "Medias", main = "Histograma para Medias, n=15", 
        freq = FALSE)
    hist(mat[, 3], xlab = "Medias", main = "Histograma para Medias, n=20", 
        freq = FALSE)
    hist(mat[, 4], xlab = "Medias", main = "Histograma para Medias, n=30", 
        freq = FALSE)
    hist(mat[, 5], xlab = "Medias", main = "Histograma para Medias, n=60", 
        freq = FALSE)
    hist(mat[, 6], xlab = "Medias", main = "Histograma para Medias, n=100", 
        freq = FALSE)
    hist(mat[, 7], xlab = "Medias", main = "Histograma para Medias, n=200", 
        freq = FALSE)
    hist(mat[, 8], xlab = "Medias", main = "Histograma para Medias, n=500", 
        freq = FALSE)
    hist(mat[, 9], xlab = "Medias", main = "Histograma para Medias, n=1000", 
        freq = FALSE)
}

tlcexp <- function(lam) 
{
    n <- c(10, 15, 20, 30, 60, 100, 200, 500, 1000)
    mat <- matrix(0, 1000, length(n))
    for (j in 1:length(n)) {
        for (i in 1:1000) {
            aux <- rexp(n[j], lam)
            med <- mean(aux)
            mu = 1/lam
            sd = 1/lam
            mat[i, j] <- sqrt(n[j]) * (med - mu)/sd
        }
    }
    par(mfrow = c(3, 3))
    hist(mat[, 1], xlab = "Medias", main = "Histograma para Medias, n=10", 
        freq = FALSE)
    hist(mat[, 2], xlab = "Medias", main = "Histograma para Medias, n=15", 
        freq = FALSE)
    hist(mat[, 3], xlab = "Medias", main = "Histograma para Medias, n=20", 
        freq = FALSE)
    hist(mat[, 4], xlab = "Medias", main = "Histograma para Medias, n=30", 
        freq = FALSE)
    hist(mat[, 5], xlab = "Medias", main = "Histograma para Medias, n=60", 
        freq = FALSE)
    hist(mat[, 6], xlab = "Medias", main = "Histograma para Medias, n=100", 
        freq = FALSE)
    hist(mat[, 7], xlab = "Medias", main = "Histograma para Medias, n=200", 
        freq = FALSE)
    hist(mat[, 8], xlab = "Medias", main = "Histograma para Medias, n=500", 
        freq = FALSE)
    hist(mat[, 9], xlab = "Medias", main = "Histograma para Medias, n=1000", 
        freq = FALSE)
}

tlcpois <- function(lam) 
{
    n <- c(10, 15, 20, 30, 60, 100, 200, 500, 1000)
    mat <- matrix(0, 1000, length(n))
    for (j in 1:length(n)) {
        for (i in 1:1000) {
            aux <- rpois(n[j], lam)
            med <- mean(aux)
            mat[i, j] <- sqrt(n[j]) * (med - lam)/sqrt(lam)
        }
    }
    par(mfrow = c(3, 3))
    hist(mat[, 1], xlab = "Medias", main = "Histograma para Medias, n=10", 
        freq = FALSE)
    hist(mat[, 2], xlab = "Medias", main = "Histograma para Medias, n=15", 
        freq = FALSE)
    hist(mat[, 3], xlab = "Medias", main = "Histograma para Medias, n=20", 
        freq = FALSE)
    hist(mat[, 4], xlab = "Medias", main = "Histograma para Medias, n=30", 
        freq = FALSE)
    hist(mat[, 5], xlab = "Medias", main = "Histograma para Medias, n=60", 
        freq = FALSE)
    hist(mat[, 6], xlab = "Medias", main = "Histograma para Medias, n=100", 
        freq = FALSE)
    hist(mat[, 7], xlab = "Medias", main = "Histograma para Medias, n=200", 
        freq = FALSE)
    hist(mat[, 8], xlab = "Medias", main = "Histograma para Medias, n=500", 
        freq = FALSE)
    hist(mat[, 9], xlab = "Medias", main = "Histograma para Medias, n=1000", 
        freq = FALSE)
}

tlcunif <- function(a, b) 
{
    n <- c(10, 15, 20, 30, 60, 100, 200, 500, 1000)
    mat <- matrix(0, 1000, length(n))
    for (j in 1:length(n)) {
        for (i in 1:1000) {
            aux <- runif(n[j], a, b)
            med <- mean(aux)
            mu = (a + b)/2
            sd = sqrt(((b - a)^2)/12)
            mat[i, j] <- sqrt(n[j]) * (med - mu)/sd
        }
    }
    par(mfrow = c(3, 3))
    hist(mat[, 1], xlab = "Medias", main = "Histograma para Medias, n=10", 
        freq = FALSE)
    hist(mat[, 2], xlab = "Medias", main = "Histograma para Medias, n=15", 
        freq = FALSE)
    hist(mat[, 3], xlab = "Medias", main = "Histograma para Medias, n=20", 
        freq = FALSE)
    hist(mat[, 4], xlab = "Medias", main = "Histograma para Medias, n=30", 
        freq = FALSE)
    hist(mat[, 5], xlab = "Medias", main = "Histograma para Medias, n=60", 
        freq = FALSE)
    hist(mat[, 6], xlab = "Medias", main = "Histograma para Medias, n=100", 
        freq = FALSE)
    hist(mat[, 7], xlab = "Medias", main = "Histograma para Medias, n=200", 
        freq = FALSE)
    hist(mat[, 8], xlab = "Medias", main = "Histograma para Medias, n=500", 
        freq = FALSE)
    hist(mat[, 9], xlab = "Medias", main = "Histograma para Medias, n=1000", 
        freq = FALSE)
}
```


```r
#TLC DE BINOMIAL CON Probabilidad 0.2
tlcbin(0.2)
```

![](index_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


