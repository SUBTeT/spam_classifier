library(rtweet)
library(rjumanpp)

appname <- "ham_or_spam"
key <- "D0KoL9TUKDV4ZP63aNgwhW8Rt"
secret <- "udEHl8ICwDPfHJlYafM1JmKhCddxuDVltXqTqXE63lxu0qjEt7"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret
)

tryCatch(
	jum_start_server(),
	error=function(e){ print("jum start server() didn't function.") }
)	
	
jum_wakati_i <- function(x){jum_wakati(gsub("[0-9０-９a-zA-Zａ-ｚ()#%$'\"=~.&,@△▲▼☆_:;/￣。、→！？♪】【『』「」!⇒¥?\r\n|\n|\r|\U0001F600-\U0001F64F|\U0001F321-\U000203FA|\U0001F32D-\U0001F9C0|-]", "", x), 1, pos="名詞|動詞", server=TRUE)}

spam <- search_tweets(q="ネットビジネス", n=100, token=twitter_token)
spam <- spam[spam$source=="twittbot.net" | spam$source=="autotweety.net", ]
spam <- get_timeline(spam$screen_name, n=10)
spam <- apply(spam[5], 1, jum_wakati_i)
spam <- gsub("ある|する|思う|いう|もの|ようだ|のだ|なる|こと|僕|何|んだ|私|できる|ため|事|やる|いる|です|言う", "", spam)
write.csv(spam, "~/temp/spam.csv", fileEncoding = "CP932")
