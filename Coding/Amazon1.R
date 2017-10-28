library(tm) #Text Mining Package
library(readr)
Andy_Weir_The_Martian <- read_csv("~/Masters Projet/Amazon-book reviews/Andy-Weir-The-Martian.csv", 
                                       col_names = FALSE)
## Amazon Review Dataset File 1
length(Andy_Weir_The_Martian)
head(Andy_Weir_The_Martian)
tail(Andy_Weir_The_Martian)
Andy = Andy_Weir_The_Martian[-(1:2)]
Andy= Andy[-(30:length(Andy))]
length(Andy)

doc.vec <- VectorSource(Andy) # Construct a Source of Vector as input
doc.corpus <- Corpus(doc.vec) # Corpus- Function for Natural Language document
summary(doc.corpus)
#tm_map: function for transformation of document
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus,removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))

library(SnowballC) #Comparison of Vocabulary
doc.corpus <- tm_map(doc.corpus, stemDocument)
doc.corpus <- tm_map(doc.corpus, stripWhitespace)
inspect(doc.corpus[8])
inspect(doc.corpus[8])
TDM <- TermDocumentMatrix(doc.corpus) # row: terms(words) column: document(webpages)
TDM
inspect(TDM[1:10,1:10])
DTM <- DocumentTermMatrix(doc.corpus) # row: document(webpages) column: terms(words)
inspect(DTM[1:10,1:10])
findFreqTerms(TDM,1500)  #each of this words occurred more than 1500 times
findAssocs(TDM, "like", 0.7)
TDM.common = removeSparseTerms(TDM,0.1) # commonly occuring terms
dim(TDM) # dimensions
dim(TDM.common)
inspect(TDM.common[1:10,1:10])

library(slam)
TDM.dense <- as.matrix(TDM.common) # attempt to turn its argument into matrix
TDM.dense
# Object.size provides an estimate of the memory that is being used to store an R object
object.size(TDM.common)
object.size(TDM.dense)

library(wordcloud)
library(RColorBrewer)
palette <- brewer.pal(9,"PuBuGn")[-(1:4)] #brewer.pal: function for color Palette

wordcloud(rownames(TDM.dense), rowSums(TDM.dense), min.freq = 1, max.words = 100,color= palette)

#Convert dtm to matrix
m <- as.matrix(DTM)
#Shorten rownames for display purposes
rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
                      substring(rownames(m),
                    nchar(rownames(m))-12,nchar(rownames(m))-4))  #paste function: concatenate vectors after converting to character.
#Compute distance between document vectors
d <- dist(m)

#Run Hierarchical Clustering using Ward's method

suppressPackageStartupMessages(library(dendextend))

library(cluster)
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)

# Plot with Ward.D method
group <- hclust(d,method = "ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
dend<- as.dendrogram(group)
dend %>% set("branches_k_color") %>% plot( main="ward.D")
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)

# Plot with Ward.D2 method
group1 <- hclust(d,method = "ward.D2")
#plot dendogram, use hang to ensure that labels fall below tree
dend<- as.dendrogram(group1)
dend %>% set("branches_k_color") %>% plot( main="ward.D2")
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)

# Plot with Single method
group2 <- hclust(d,method = "single")
#plot dendogram, use hang to ensure that labels fall below tree
dend<- as.dendrogram(group2)
dend %>% set("branches_k_color") %>% plot( main="Single")
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)

# Plot with Complete method
group3 <- hclust(d,method = "complete")
#plot dendogram, use hang to ensure that labels fall below tree
dend<- as.dendrogram(group3)
dend %>% set("branches_k_color") %>% plot( main="Complete")
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)

# Plot with Average method
group4 <- hclust(d,method = "average")
#plot dendogram, use hang to ensure that labels fall below tree
dend<- as.dendrogram(group4)
dend %>% set("branches_k_color") %>% plot( main="Average")
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)

# Plot with mcquitty method
group5 <- hclust(d,method = "mcquitty")
#plot dendogram, use hang to ensure that labels fall below tree
dend<- as.dendrogram(group5)
dend %>% set("branches_k_color") %>% plot( main="Mcquitty")
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)

# Plot with median method
group6 <- hclust(d,method = "median")
#plot dendogram, use hang to ensure that labels fall below tree
dend<- as.dendrogram(group6)
dend %>% set("branches_k_color") %>% plot( main="Median")
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)

# Plot with centroid method
group7 <- hclust(d,method = "centroid")
#plot dendogram, use hang to ensure that labels fall below tree
dend<- as.dendrogram(group7)
dend %>% set("branches_k_color") %>% plot( main="Centroid")
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 10, lty = 15, lwd = 20)

