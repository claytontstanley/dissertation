# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = tools:::file_path_as_absolute(dirname(frameFiles[[length(frameFiles)]]))

library(stringr)

print(PATH)

library(RMySQL)
con = dbConnect(MySQL())
idUserIdFrm=dbGetQuery(con, 'select id, ownerUserId as userId from sotero.posts where posttypeid = 1 and ownerUserId is not NULL')
write.csv(idUserIdFrm, file=str_c(PATH, "/", "idUserId.csv"), row.names=F)
dbDisconnect(con)
detach(package:RMySQL, unload=T)

library(RPostgreSQL)
con = dbConnect(PostgreSQL())

fileName <- str_c(PATH, '/createIdUserId.sql')
sqlStr = readChar(fileName, file.info(fileName)$size)
rs = dbSendQuery(con, sqlStr)

fileName = str_c(PATH, '/idUserId.csv')
sqlStr = str_c("copy idUserId (id, userId) from '", fileName, "' delimiters ',' csv header")
rs = dbSendQuery(con, sqlStr)

dbDisconnect(con)
detach(package:RPostgreSQL, unload=T)


