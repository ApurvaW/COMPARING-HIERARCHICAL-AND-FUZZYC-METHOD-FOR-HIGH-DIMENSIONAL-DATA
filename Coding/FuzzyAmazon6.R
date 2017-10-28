library(tm) #Text Mining Package
library(readr)
Laura_Hillenbrand_Unbroken <- read_csv("~/Masters Projet/Amazon-book reviews/Laura-Hillenbrand-Unbroken.csv", 
                                       col_names = FALSE)

## Amazon Review Dataset File 6
length(Laura_Hillenbrand_Unbroken)
head(Laura_Hillenbrand_Unbroken)
tail(Laura_Hillenbrand_Unbroken)
summary(Laura_Hillenbrand_Unbroken)
Laura = Laura_Hillenbrand_Unbroken[-(1)]
length(Laura)

doc.vec <- VectorSource(Laura) # Construct a Source of Vector as input
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
inspect(doc.corpus[11]) # paragraph from doc. corpus of column no. given 
inspect(doc.corpus[11])
TDM <- TermDocumentMatrix(doc.corpus) # row: terms(words) column: document(webpages)
TDM
inspect(TDM[1:11,1:11])
DTM <- DocumentTermMatrix(doc.corpus) # row: document(webpages) column: terms(words)
DTM
inspect(DTM[1:11,1:11])
findFreqTerms(TDM,700)  #each of this words occurred more than 1000 times
findAssocs(TDM, "like", 0.9) #range between 0-1
TDM.common = removeSparseTerms(TDM,0.9) # commonly occuring terms with allowed less sparsity between range 0-1
dim(TDM) # dimensions
dim(TDM.common)
inspect(TDM.common[1:11,1:11])

library(slam)
TDM.dense <- as.matrix(TDM) # attempt to turn its argument into matrix
TDM.dense
#summary(TDM.dense)

# Object.size provides an estimate of the memory that is being used to store an R object
object.size(TDM.common)
object.size(TDM.dense)

library(wordcloud)
library(RColorBrewer)
palette <- brewer.pal(9,"PuRd")[-(1:4)] #brewer.pal: function for color Palette
wordcloud(rownames(TDM.dense), rowSums(TDM.dense), min.freq = 1,max.words = 100, color= palette)

#Convert dtm to matrix
m <- as.matrix(DTM)

library(e1071)
library(scatterplot3d)
x <- rbind(m)
x <-t(x)
result <- cmeans(x,7,25876,verbose = TRUE,method = "cmeans")
print(result)
s3d <- scatterplot3d(result$membership, color = result$cluster, type = "h", angle = 55, scale.y = 0.7, pch = 16, main = "Pertinence for Fuzzy Amazon Dataset File 6")
plot(m, col=result$cluster)


