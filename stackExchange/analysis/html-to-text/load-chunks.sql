load data local infile 'chunks-huge.csv' into table chunks fields terminated by ','
enclosed by '"'
ESCAPED BY ''
(ChunkId, Id, Chunk, ChunkType)
