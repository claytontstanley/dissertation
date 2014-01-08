library(RPostgreSQL)
library(sqldf)
library(data.table)
PATH = getPathToThisFile()

options(sqldf.RPostgreSQL.user = 'claytonstanley',
	sqldf.RPostgreSQL.dbname = 'claytonstanley')
options("scipen"=100, "digits"=4)

sqldf("select * from tweets where user_screen_name = 'claytonstanley1'")
sqldf('select count(*) from tweets')
sqldf('select count(*) from topUsers')
sqldf('delete from topUsers')
data.table(sqldf('select t.user_screen_name,rank from tweets as t join topusers as u on t.user_screen_name = u.user_screen_name group by t.user_screen_name,u.rank order by rank'))
data.table(sqldf('select * from topusers order by rank'))
sqldf("select * from tweets where user_screen_name='jlo'")
sqldf('select user_screen_name,count(*) from tweets group by user_screen_name')
sqldf('select lang,count(*) as count from tweets group by lang order by count desc')
sqldf('select retweeted,count(*) as count from tweets group by retweeted order by count desc')
sqldf('select truncated,count(*) as count from tweets group by truncated order by count desc')
sqldf("select * from topUsers")[1:100,]
data.table(sqldf("select * from twitter_users"))

tweetsTbl
tweetsTbl = data.table(sqldf("select * from tweets"))
tweetsTbl[, list(N=.N), by=retweeted]
tweetsTbl[, .N, by=lang]
tables()
.ls.objects()
gc()
tweetsTbl[, maxID := max(id), by=user_screen_name]
tweetsTbl[, minID := min(id), by=user_screen_name]

tweetsTbl[, containsHashtag := 0]
tweetsTbl[grepl('#', text), containsHashtag := 1]
tweetsTbl[, list(numWithHashtags=sum(containsHashtag), numOfTweets=.N), by=user_screen_name][, propWithHashtags := numWithHashtags/numOfTweets][,][,hist(propWithHashtags)]
tweetsTbl

#sqldf("select user_screen_name,id,created_at,text from tweets where user_screen_name = 'katyperry' order by id desc limit 10")
#sqldf("select user_screen_name,id,created_at,text from tweets where user_screen_name = 'BarackObama' order by id desc limit 10")
#sqldf("select rank, t.user_screen_name, count(*) from tweets as t join topUsers as u on t.user_screen_name = u.user_screen_name group by t.user_screen_name, rank order by rank desc")
#write.csv(tweetsTbl, file=str_c(PATH, "/currentTweets.csv"))
#tweetsTbl
#tweetsTbl[in_reply_to_status_id != "",]


