library(tm)
library(e1071)
library(gmodels)

convert_counts <- function(x){x <- ifelse(x>0, "Yes", "No")}


f <- read.csv("~/r-datasets/tweet.csv", fileEncoding = "CP932")
f <- f[sample(nrow(f)),]
f_corpus <- VCorpus(VectorSource(f$text))
f_dtm <- DocumentTermMatrix(f_corpus, control=list(wordLengths=c(1, 15)))

int  <- as.integer(length(f_corpus)*0.75)
int_plus <- int + 1
last <- length(f_corpus)

f_train <- f_dtm[1:int, ]
f_test <- f_dtm[int_plus:last, ]
f_train_labels <- f[1:int, ]$type
f_test_labels <- f[int_plus:last, ]$type
freq_words <- findFreqTerms(f_train, 5)
f_dtm_freq_train <- f_train[, freq_words]
f_dtm_freq_test <- f_test[, freq_words]
f_train <- apply(f_dtm_freq_train, MARGIN=2, convert_counts)
f_test <- apply(f_dtm_freq_test, MARGIN=2, convert_counts)
classifier <- naiveBayes(f_train, f_train_labels, laplace=1)
pred <- predict(classifier, f_test)
CrossTable(pred, f_test_labels, prop.chisq=FALSE, prop.t=FALSE, dnn=c("predicted", "actual"))
