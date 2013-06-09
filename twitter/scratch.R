consumerKey = "vKbz24SqytZnYO33FNkR7w"
consumerSecret = "jjobro8Chy9aKMzo8szYMz9tHftONLRkjNnrxk0"
twitCred = getTwitterOAuth(consumerKey, consumerSecret)

save(list="twitCred", file="~/twitteR_credentials")

load("~/twitteR_credentials")

registerTwitterOAuth(twitCred)



