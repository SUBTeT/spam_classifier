library(tm)
library(rtweet)
library(wordcloud)

test <- read.csv("~/r-datasets/tweet.csv", fileEncoding = "CP932")
spam <- subset(test, type %in% "spam")
spam_corpus <- VCorpus(VectorSource(spam$text))
spam_dtm <- DocumentTermMatrix(spam_corpus, control=list(wordLengths=c(1, 15)))
spam_mat <- as.matrix(spam_dtm)
freq <- data.frame(apply(spam_mat, 2, sum))
wordcloud(colnames(spam_mat), freq$apply.spam_mat..2..sum., max.words=50, random.order=TRUE, color=rainbow(20),scale=c(3,1), random.color=FALSE)