load data local infile 'chunks.csv' into table chunks fields terminated by ','
enclosed by '"'
ESCAPED BY '\\'
(ChunkId, Id, Chunk, ChunkType)
