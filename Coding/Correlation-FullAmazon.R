library(tm) #Text Mining Package
library(readr)
Andy_Weir_The_Martian <- read_csv("~/Masters Projet/Amazon-book reviews/Andy-Weir-The-Martian.csv", 
                                  col_names = FALSE)

library(dplyr)
library(tidytext)
library(janeaustenr)

cname <- file.path("C:/Users/Owner/Documents/Masters Projet/Amazon-book reviews")
dir(cname)

docs<- VCorpus(DirSource(cname))
summary(docs)
inspect(docs[1])
writeLines(as.character(docs[1]))

docs<- tm_map(docs, removePunctuation)

for(j in seq(docs)) {
  docs[[j]] <- gsub("/"," ",docs[[j]])
  docs[[j]] <- gsub("@"," ",docs[[j]])
  docs[[j]] <- gsub("\\|"," ",docs[[j]])
  docs[[j]] <- gsub("\u2028"," ",docs[[j]])
}

docs <-  tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs

docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removeWords, c("syllogism", "tautology"))

for(j in seq(docs)) {
  docs[[j]] <- gsub("fake news","fake_news",docs[[j]])
  docs[[j]] <- gsub("inner city","inner-city",docs[[j]])
  docs[[j]] <- gsub("politically correct","politically_correct",docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)
docs_st <- tm_map(docs, stemDocument)
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))

#docs_stc <- tm_map(docs_stc, stemCompletion, dictionary = DocsCopy, lazy= TRUE)
#docs_stc <- tm_map(docs_stc, PlainTextDocument)
#writeLines(as.character(docs_stc[1]))

docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)
dtm

tdm <- TermDocumentMatrix(docs)
tdm

freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq)
m <- as.matrix(dtm)
dim(m)
write.csv(m, file = "DocumentTermMatrix.csv")

dtms <- removeSparseTerms(dtm, 0.2)
dtms

freq <- colSums(as.matrix(dtm))
head(table(freq), 30)
tail(table(freq), 30)

freq <- colSums(as.matrix(dtms))
freq

freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
head(freq, 15)
findFreqTerms(dtm, lowfreq = 100)

wf <- data.frame(word= names(freq), freq=freq)
head(wf)

library(ggplot2)
p <- ggplot(subset(wf, freq>50), aes(x= reorder(word, -freq), y= freq)) +
      geom_bar(stat = "identity")+
     theme(axis.title.x = element_text(angle = 45, hjust = 1))
p

findAssocs(dtm, c("country","american"), corlimit = 0.85)
findAssocs(dtms, "think", corlimit = 0.70)

library(wordcloud)
library(RColorBrewer)

set.seed(142)
#wordcloud(names(freq), freq, min.freq = 25)

#wordcloud(names(freq), freq, max.words = 100)

wordcloud(names(freq), freq, min.freq = 20, scale = c(5, 0.1), colors = brewer.pal(9, "PuBuGn"))

dtmss <- removeSparseTerms(dtm, 0.15)
dtmss

library(cluster)
library(factoextra)
library(colorspace)
library(dendextend)

d <- dist(t(dtmss), method = "euclidean")
fit <- hclust(d= d, method = "complete")
fit
plot(fit, hang=-1)

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7)