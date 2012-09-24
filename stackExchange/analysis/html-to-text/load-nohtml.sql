load data local infile 'nohtml.csv' into table posts_nohtml fields terminated by ','
enclosed by '"'
ESCAPED BY '\\'
(Id, Body)
