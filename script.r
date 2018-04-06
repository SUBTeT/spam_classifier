library(tm)
library(e1071)
library(rtweet)
library(gmodels)
library(rjumanpp)

tryCatch(
jum_start_server(),
error=function(e){ print("jum start server() didn't function.") }
)

jum_wakati_i <- function(x){jum_wakati(gsub("[0-9０-９a-zA-Zａ-ｚ()#%$'\"=~.&,@△▲▼☆_:;/￣。、→！？♪】【『』「」!⇒¥?\r\n|\n|\r|\U0001F600-\U0001F64F|\U0001F321-\U000203FA|\U0001F32D-\U0001F9C0|-]", "", x), 1, pos="名詞|動詞", server=TRUE)}

convert_counts <- function(x){x <- ifelse(x >0, "Yes", "No")}

test <- read.csv("~/r-datasets/tweet.csv", fileEncoding = "CP932")
test <- test[sample(nrow(test)),]
test_train_labels <- test$type
test_corpus <- VCorpus(VectorSource(test$text))
test_dtm <- DocumentTermMatrix(test_corpus, control=list(wordLengths=c(1, 15)))
test_freq_words <- findFreqTerms(test_dtm, 5)
test_dtm_freq <- test_dtm[, test_freq_words]
test_train <- apply(test_dtm_freq, MARGIN=2, convert_counts)
classifier <- naiveBayes(test_train, test_train_labels, laplace=1)
print("classifier success")

e <- subset(stream_tweets(q="", timeout=200), lang %in% "ja")
temp_wakati <- apply(e[5], 1, jum_wakati_i)
temp_wakati <- gsub("ある|する|思う|いう|もの|ようだ|のだ|なる|こと|僕|何|んだ|私|できる|ため|事|やる|いる|です|言う", "", temp_wakati)
f <- data.frame(temp_wakati, e$screen_name)
f <- f[sample(nrow(f)),]
f_corpus <- VCorpus(VectorSource(f$temp_wakati))
f_dtm <- DocumentTermMatrix(f_corpus, control=list(wordLengths=c(1, 15)))
freq_words <- findFreqTerms(f_dtm, 5)
f_dtm_freq <- f_dtm[, freq_words]
f_data <- apply(f_dtm_freq, MARGIN=2, convert_counts)
pred <- predict(classifier, f_data)
g <- data.frame(f, pred)
h <- subset(g, g$pred=="spam")

write.csv(h, "~/temp/prediction.csv", fileEncoding = "CP932")
