data(iris)

library(cluster)
library(ggplot2)
library(colorspace)
library(factoextra)
library(dendextend)

fannyx <- fanny(iris,7)
fannyx
summary(fannyx)
plot(fannyx)
return()