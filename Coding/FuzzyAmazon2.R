## Fuzzy Clustering 
library(e1071)
library(tm) #Text Mining Package
library(scatterplot3d)
library(readr)
Donna_Tartt_The_Goldfinch <- read_csv("~/Masters Projet/Amazon-book reviews/Donna-Tartt-The-Goldfinch.csv", 
                                      col_names = FALSE)
## Amazon Review Dataset File 1
length(Donna_Tartt_The_Goldfinch)
head(Donna_Tartt_The_Goldfinch)
tail(Donna_Tartt_The_Goldfinch)
summary(Donna_Tartt_The_Goldfinch)
Donna = Donna_Tartt_The_Goldfinch[-(1)]
length(Donna)

doc.vec <- VectorSource(Donna) # Construct a Source of Vector as input
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
inspect(doc.corpus[12]) # paragraph from doc. corpus of column no. given 
inspect(doc.corpus[8])
TDM <- TermDocumentMatrix(doc.corpus) # row: terms(words) column: document(webpages)
TDM
inspect(TDM[1:15,1:15])
DTM <- DocumentTermMatrix(doc.corpus) # row: document(webpages) column: terms(words)
DTM
inspect(DTM[1:15,1:15])
findFreqTerms(TDM,700)  #each of this words occurred more than 1000 times
findAssocs(TDM, "like", 0.8) #range between 0-1
TDM.common = removeSparseTerms(TDM,0.1) # commonly occuring terms with allowed less sparsity between range 0-1
dim(TDM) # dimensions
dim(TDM.common)
inspect(TDM.common[1:15,1:15])

library(slam)
TDM.dense <- as.matrix(TDM.common) # attempt to turn its argument into matrix
TDM.dense
#summary(TDM.dense)

# Object.size provides an estimate of the memory that is being used to store an R object
object.size(TDM.common)
object.size(TDM.dense)

library(wordcloud)
library(RColorBrewer)
palette <- brewer.pal(9,"Purples")[-(1:4)] #brewer.pal: function for color Palette
wordcloud(rownames(TDM.dense), rowSums(TDM.dense), min.freq = 1, color= palette)

#Convert dtm to matrix
m <- as.matrix(DTM)

x <- rbind(m)
x <-t(x)
result <- cmeans(x,7,22861,verbose = TRUE,method = "cmeans")
print(result)
s3d <- scatterplot3d(result$membership, color = result$cluster, type = "h", angle = 55, scale.y = 0.7, pch = 16, main = "Pertinence")
plot(m, col=result$cluster)