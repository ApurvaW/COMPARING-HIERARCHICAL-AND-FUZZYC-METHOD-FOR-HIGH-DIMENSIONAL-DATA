library(readxl)
library(tm) #Text Mining Package

Online_Retail <- read_excel("~/Masters Projet/Online Retail/Online Retail.xlsx")

#Online_Retail$InvoiceDate <- as.Date(Online_Retail$InvoiceDate, format="%Y%m%d")
View(Online_Retail)
names(Online_Retail)
new <- Online_Retail[-5]
names(new)
length(new)
head(new)
tail(new)
summary(new)

doc.vec <- VectorSource(new) # Construct a Source of Vector as input
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
TDM <- TermDocumentMatrix(doc.corpus, control = list(wordLengths=c(0,Inf))) # row: terms(words) column: document(webpages)
TDM
inspect(TDM[1:3,1:3])
DTM <- DocumentTermMatrix(doc.corpus) # row: document(webpages) column: terms(words)
DTM
inspect(DTM[1:3,1:3])
findFreqTerms(TDM,700)  #each of this words occurred more than 1000 times
findAssocs(TDM, "like", 0.9) #range between 0-1
TDM.common = removeSparseTerms(TDM,0.9) # commonly occuring terms with allowed less sparsity between range 0-1
dim(TDM) # dimensions
dim(TDM.common)
#inspect(TDM.common[1:1,1:1])

library(slam)
TDM.dense <- as.matrix(TDM) # attempt to turn its argument into matrix
TDM.dense
#summary(TDM.dense)

# Object.size provides an estimate of the memory that is being used to store an R object
object.size(TDM.common)
object.size(TDM.dense)

library(wordcloud)
library(RColorBrewer)
palette <- brewer.pal(9,"YlOrBr")[-(1:7)]#brewer.pal: function for color Palette
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
