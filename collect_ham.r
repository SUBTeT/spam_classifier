library(rtweet)
library(rjumanpp)

tryCatch(
	jum_start_server(),
	error=function(e){ print("jum start server() didn't function.") }
)
	
jum_wakati_i <- function(x){jum_wakati(gsub("[0-9０-９a-zA-Zａ-ｚ()#%$'\"=~.&,@△▲▼☆_:;/￣。、→！？♪】【『』「」!⇒¥?\r\n|\n|\r|\U0001F600-\U0001F64F|\U0001F321-\U000203FA|\U0001F32D-\U0001F9C0|-]", "", x), 1, pos="名詞|動詞", server=TRUE)}

search_word <- c("Twitter for iPhone", "Twitter for Android", "Twitter for iPad", "Twitter Lite", "Twitter Web Client", "feather for iOS", "Tabtter Free", "TweetDeck", "Janetter for Android")
e <- subset(stream_tweets(q="", timeout=200), lang %in% "ja")
e <- subset(e, source %in% search_word)
temp_wakati <- apply(e[5], 1, jum_wakati_i)
temp_wakati <- gsub("ある|する|思う|いう|もの|ようだ|のだ|なる|こと|僕|何|んだ|私|できる|ため|事|やる|いる|です|言う", "", temp_wakati)
object <- data.frame(temp_wakati, e$screen_name)
write.csv(object, "~/temp/ham.csv", fileEncoding = "CP932")

