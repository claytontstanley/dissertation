load data local infile 'chunks-huge.csv' into table chunks fields terminated by ','
enclosed by '"'
ESCAPED BY ''
(ChunkId, Id, Chunk, ChunkType);

load data local infile 'tag/nlp/nlp.csv' into table subsets fields terminated by ','
enclosed by '"'
ESCAPED BY ''
(Id)
set subset = "tag";

load data local infile 'title/nlp/nlp.csv' into table subsets fields terminated by ','
enclosed by '"'
ESCAPED BY ''
(Id)
set subset = "title";

load data local infile 'tag-subset-1/nlp-huge/nlp-huge.csv' into table subsets fields terminated by ','
enclosed by '"'
ESCAPED BY ''
(Id)
set subset = "tag-subset-1";

load data local infile 'title-subset-1/nlp-huge/nlp-huge.csv' into table subsets fields terminated by ','
enclosed by '"'
ESCAPED BY ''
(Id)
set subset = "title-subset-1";


