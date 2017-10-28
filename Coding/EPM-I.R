library(cluster)
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)
library(readxl)
## EPM intermediate Dataset
intermediate_grades <- read_excel("~/Masters Projet/EPM/Data/intermediate_grades.xlsx")
View(intermediate_grades)
pca <- princomp(intermediate_grades[, 2:6], cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
PrincipalComponent1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
PrincipalComponent2 <- -1*pc.comp[,2]
clustering.data <- cbind(PrincipalComponent1, PrincipalComponent2)
plot(PrincipalComponent1, PrincipalComponent2)

# ward.D Link
d <- dist(clustering.data)
hclusters <- hclust(d, method = "ward.D")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, intermediate_grades$`Student Id`)
aggregate(intermediate_grades[,2:6],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)
dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "ward.D Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# ward.D2 Link
d <- dist(clustering.data)
hclusters <- hclust(d, method = "ward.D2")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, intermediate_grades$`Student Id`)
aggregate(intermediate_grades[,2:6],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)
dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "ward.D2 Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

#Single link
d <- dist(clustering.data)
hclusters <- hclust(d , method = "single")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, intermediate_grades$`Student Id`)
aggregate(iris[,2:6],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)
dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Single Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

#Complete link
d <- dist(clustering.data)
hclusters <- hclust(d, method = "complete")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, intermediate_grades$`Student Id`)
aggregate(iris[,2:6],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)
dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Complete Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# Average Link
d <- dist(clustering.data)
hclusters <- hclust(d, method = "Average")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, intermediate_grades$`Student Id`)
aggregate(intermediate_grades[,2:6],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)
dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Average Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# mcquitty Link
d <- dist(clustering.data)
hclusters <- hclust(d, method = "mcquitty")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, intermediate_grades$`Student Id`)
aggregate(intermediate_grades[,2:6],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)
dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Mcquitty Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# median Link
d <- dist(clustering.data)
hclusters <- hclust(d, method = "median")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, intermediate_grades$`Student Id`)
aggregate(intermediate_grades[,2:6],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)
dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Median Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# centroid Link
d <- dist(clustering.data)
hclusters <- hclust(d, method = "centroid")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, intermediate_grades$`Student Id`)
aggregate(intermediate_grades[,2:6],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)
dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Centroid Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

#fviz_cluster(hclusters, data = clustering.data)

#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)

## Fuzzy Clustering 

#fannyx <- fanny(intermediate_grades,7)
#fannyx
#summary(fannyx)
#plot(fannyx)
#return()