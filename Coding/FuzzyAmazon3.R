library(tm)
library(readr)
EL_James_Fifty_Shades_of_Grey <- read_csv("~/Masters Projet/Amazon-book reviews/EL-James-Fifty-Shades-of-Grey.csv", 
                                          col_names = FALSE)
## Amazon Review Dataset File 3
length(EL_James_Fifty_Shades_of_Grey)
head(EL_James_Fifty_Shades_of_Grey)
tail(EL_James_Fifty_Shades_of_Grey)
summary(EL_James_Fifty_Shades_of_Grey)
El = EL_James_Fifty_Shades_of_Grey[-(1)]
length(El)

doc.vec <- VectorSource(El) # Construct a Source of Vector as input
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
inspect(doc.corpus[10])

TDM <- TermDocumentMatrix(doc.corpus) # row: terms(words) column: document(webpages)
TDM
inspect(TDM[1:19,1:19])

DTM <- DocumentTermMatrix(doc.corpus) # row: document(webpages) column: terms(words)
DTM
inspect(DTM[1:19,1:19])

findFreqTerms(TDM,500)  #each of this words occurred more than 500 times
findAssocs(TDM, "like", 0.9) #range between 0-1
TDM.common = removeSparseTerms(TDM,0.9) # commonly occuring terms with allowed less sparsity between range 0-1
dim(TDM) # dimensions
dim(TDM.common)
inspect(TDM.common[1:19,1:19])

library(slam)
TDM.dense <- as.matrix(TDM) # attempt to turn its argument into matrix
TDM.dense
#summary(TDM.dense)

# Object.size provides an estimate of the memory that is being used to store an R object
object.size(TDM.common)
object.size(TDM.dense)

library(wordcloud)
library(RColorBrewer)
palette <- brewer.pal(9,"YlGnBu")[-(1:4)] #brewer.pal: function for color Palette
wordcloud(rownames(TDM.dense), rowSums(TDM.dense), min.freq = 1, color= palette)

#Convert dtm to matrix
m <- as.matrix(DTM)

library(scatterplot3d)
x <- rbind(m)
x <-t(x)
result <- cmeans(x,7,32977,verbose = TRUE,method = "cmeans")
print(result)
s3d <- scatterplot3d(result$membership, color = result$cluster, type = "h", angle = 55, scale.y = 0.7, pch = 16, main = "Pertinence for Fuzzy Amazon Dataset File 3.")
plot(m, col=result$cluster)
