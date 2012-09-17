TOP := $(shell pwd)

sqlcmd = mysql -u root -p
#xmlDir = /Users/claytonstanley/Documents/Vuze Downloads/Stack Exchange Data Dump - Sept 2011/Content/092011 Stack Overflow
xmlDir = /Users/claytonstanley/Documents/Vuze Downloads/Stack Overflow Data Dump - Apr 2011/Content/stackoverflow.com.7z
xmlName = badges.xml
xmlPath = ${xmlDir}/${xmlName}
sqlDir = ${xmlDir}
sqlName = $(subst .xml,.sql,${xmlName})
sqlPath = ${sqlDir}/${sqlName}

clear:
	cat mysql.sql | ${sqlcmd}

load:
	cat before-posts.sql "${sqlPath}" | ${sqlcmd}

convert:
	cd "${xmlDir}"; ${TOP}/xml2sql.py "${xmlName}"
