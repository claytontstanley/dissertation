drop table if exists sotero.subsets;

create table if not exists sotero.subsets (
	Id int(11) not null,
	Subset varchar(250) binary character set 'utf8' collate utf8_general_ci not null,
	primary key (Id, Subset)
 );

load data local infile 'tag/nlp/nlp.csv' into table subsets fields terminated by ','
enclosed by '"'
ESCAPED BY ''
(Id)
set subset = "tag/nlp/nlp.csv";

load data local infile 'title/nlp/nlp.csv' into table subsets fields terminated by ','
enclosed by '"'
ESCAPED BY ''
(Id)
set subset = "title/nlp/nlp.csv";

load data local infile 'tag-subset-1/nlp-huge/nlp-huge.csv' into table subsets fields terminated by ','
enclosed by '"'
ESCAPED BY ''
(Id)
set subset = "tag-subset-1/nlp-huge/nlp-huge.csv";

load data local infile 'title-subset-1/nlp-huge/nlp-huge.csv' into table subsets fields terminated by ','
enclosed by '"'
ESCAPED BY ''
(Id)
set subset = "title-subset-1/nlp-huge/nlp-huge.csv";


