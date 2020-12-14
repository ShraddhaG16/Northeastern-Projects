#PART 1
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

cname <- file.path("C:", "texts")   
cname   
dir(cname)

library(tm)

docs <- VCorpus(DirSource(cname))   
summary(docs)   

inspect(docs[1])

docs <- tm_map(docs,removePunctuation) 
docs

for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])
}

 install.packages("twitteR")
library("twitteR")
library("wordcloud")
library("SnowballC")
library("tm")

docs <- tm_map(docs, removeNumbers)  

docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs
DocsCopy

docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)
#Removing particular words
docs <- tm_map(docs, removeWords, c("syllogism", "tautology"))   

for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}

docs <- tm_map(docs, PlainTextDocument)
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))
writeLines(as.character(docs_st[2]))
writeLines(as.character(docs_st[3]))
writeLines(as.character(docs_st[4]))
writeLines(as.character(docs_st[5]))
writeLines(as.character(docs_st[6]))
writeLines(as.character(docs_st[7]))
writeLines(as.character(docs_st[8]))
writeLines(as.character(docs_st[9]))
writeLines(as.character(docs_st[10]))
writeLines(as.character(docs_st[11]))

docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
docs_stc <- tm_map(docs_st, tolower)  
docs_stc <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_stc[1]))

docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, PlainTextDocument)

#PART 2

dtm <- DocumentTermMatrix(docs)   
dtm 
tdm <- TermDocumentMatrix(docs)   
tdm  

freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq) 
m <- as.matrix(dtm)   
dim(m) 
write.csv(m, file="DocumentTermMatrix.csv")   

dtms <- removeSparseTerms(dtm, 0.2)  
dtms

freq <- colSums(as.matrix(dtm)) 
head(table(freq), 20) 
tail(table(freq), 20) 

freq <- colSums(as.matrix(dtms))   #
freq 

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)

#create a data frame for next steps
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)


#PART 3
library(ggplot2)
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
p 

findAssocs(dtm, c("world" , "great"), corlimit=0.88)
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.15)    
freq <- colSums(as.matrix(dtm)) 
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)  

dtms <- removeSparseTerms(dtm, 0.15) 


library(cluster)   
d <- dist(t(dtms), method="euclidian")  
fit <- hclust(d=d, method="complete")  
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)   
rect.hclust(fit, k=6, border="blue")

library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.15)   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 
  