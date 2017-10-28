data(dress)

#suppressPackageStartupMessages(library(dendextend))

library(cluster)
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)

pca <- princomp(dress[, 1:9], cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
PrincipalComponent1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
PrincipalComponent2 <- -1*pc.comp[,2] 

#Hierarchical Clustering
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)

#set.seed(500)
d_dress<- dist(clustering.data)
hclusters <- hclust(d_dress, method = "average")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, dress$Type)
aggregate(iris[,2:10],by=list(clusterCut),mean)
plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)


dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Average Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# Horizontal plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = " Average Cluster Dendrogram",horiz = TRUE)
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)


#Single link
d_dress<- dist(clustering.data)
hclusters <- hclust(d_dress, method = "single")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, dress$Type)
aggregate(dress [,2:10],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)

dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Single Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# Horizontal plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Single Cluster Dendrogram",horiz = TRUE)
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)



#Complete link
d_dress<- dist(clustering.data)
hclusters <- hclust(d_dress, method = "complete")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, dress$Type)
aggregate(dress[,2:10],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)

dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Complete Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# Horizontal plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Complete Cluster Dendrogram",horiz = TRUE)
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)


#fviz_cluster(hclusters, data = clustering.data)

#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)
