library(RPostgreSQL)
library(sqldf)

options(sqldf.RPostgreSQL.user = 'claytonstanley',
	sqldf.RPostgreSQL.dbname = 'claytonstanley')
options("scipen"=100, "digits"=4)

sqldf("select user_screen_name,id,created_at,text from tweets where user_screen_name = 'katyperry' order by id desc limit 10")
sqldf("select user_screen_name,id,created_at,text from tweets where user_screen_name = 'BarackObama' order by id desc limit 10")

sqldf("select rank, t.user_screen_name, count(*) from tweets as t join topUsers as u on t.user_screen_name = u.user_screen_name group by t.user_screen_name, rank order by rank desc")

sqldf("select user_screen_name,id from tweets where id = max(id) group by user_screen_name")
sqldf("select user_screen_name,id from tweets as t join (select user_screen_name,max(id) from tweets group by user_screen_name) as i on t.user_screen_name = i.user_screen_name group by user_screen_name")

sqldf("select * from tweets where id = '409332078233542656'")$text
tweetsTbl = data.table(sqldf("select * from tweets"))
tweetsTbl[, maxID := max(id), by=user_screen_name]
tweetsTbl[, minID := min(id), by=user_screen_name]

tweetsTbl[grepl('#', text),]
tweetsTbl

tweetsTbl[in_reply_to_status_id != "",]

Encoding(tweetsTbl$text)
getOption("encoding")
tweetsTbl
Sys.getlocale(category = "LC_ALL")

?sqldf

tweetsTbl[id==minID, list(id, user_id, user_screen_name, created_at)]
tweetsTbl[, .N, by=user_screen_name]


tweetsTblOrig[id==minID, list(id, user_id, user_screen_name, created_at)]
tweetsTblOrig[, .N, by=user_screen_name]

tweetsTblOrig = tweetsTbl

