library(tm) #Text Mining Package
library(readxl)
Dress <-read_excel("~/Masters Projet/Dresses-attributes_sales/Attribute DataSet.xlsx")

View(Dress)
## Dress Attribute Dataset File 
length(Dress)
head(Dress)
tail(Dress)
Dress = Dress[-(1)]
Dress= Dress[-(13:length(Dress))]
Dress <- Dress[-(3)]
length(Dress)

doc.vec <- VectorSource(Dress) # Construct a Source of Vector as input
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
inspect(TDM[1:6,1:6])
DTM <- DocumentTermMatrix(doc.corpus) # row: document(webpages) column: terms(words)
inspect(DTM[1:6,1:6])
findFreqTerms(TDM,1000)  #each of this words occurred more than 1500 times
findAssocs(TDM, "like", 0.9)
TDM.common = removeSparseTerms(TDM,0.9) # commonly occuring terms
dim(TDM) # dimensions
dim(TDM.common)
#inspect(TDM.common[1:6,1:6])

library(slam)
TDM.dense <- as.matrix(TDM) # attempt to turn its argument into matrix
TDM.dense
# Object.size provides an estimate of the memory that is being used to store an R object
object.size(TDM.common)
object.size(TDM.dense)

library(wordcloud)
library(RColorBrewer)
palette <- brewer.pal(9,"PuBuGn")[-(1:4)] #brewer.pal: function for color Palette

wordcloud(rownames(TDM.dense), rowSums(TDM.dense), min.freq = 1, color= palette)

#Convert dtm to matrix
m <- as.matrix(DTM)

library(e1071)
library(scatterplot3d)

x <- rbind(m)
x <-t(x)
result <- cmeans(x,3,50,verbose = TRUE,method = "cmeans")
print(result)
s3d <- scatterplot3d(result$membership, color = result$cluster, type = "h", angle = 55, scale.y = 0.7, pch = 16, main = "Pertinence")
plot(m, col=result$cluster)
